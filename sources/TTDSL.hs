{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module TTDSL where

import Control.Monad.Writer.Lazy

import Data.Foldable (forM_)
import Data.Vector (Vector)
import Control.Monad (replicateM, replicateM_)
import System.Random (randomIO)
import Test.HUnit (assertBool)

import SPrelude as S
import Prelude as P

type FTensor v = Tensor v Float
type FNode = FTensor Value
type FVar = Variable Float

type WriterBuild v = WriterT [FVar] Build v
instance MonadBuild (WriterT [FVar] Build) where
  build m = lift m

type Input v = FNode
type Output v = FNode

data Model i = Model { modelInputs    :: [FVar]
                     , modelVariables :: [FVar]
                     , modelOutputs   :: [FNode]
                     }

data Phant a = Phant

class Encodable a where
  getEncodingShape :: Phant a -> Shape
  encodeInTensor :: a -> TensorData Float
  
class BuildMagic i o | i -> o where
  buildWriter :: MonadBuild m => i -> m (Model o)

instance BuildMagic (WriterBuild (Output a)) a where
  buildWriter modelBuild = do
    (out, vars) <- build $ runWriterT modelBuild
    return $ Model [] vars [out]

instance BuildMagic (WriterBuild [Output a]) [a] where
  buildWriter modelBuild = do
    (out, vars) <- build $ runWriterT modelBuild
    return $ Model [] vars out

instance (BuildMagic (WriterBuild a) a', BuildMagic (WriterBuild b) b') => BuildMagic (WriterBuild (a, b)) (a',b') where
  buildWriter modelBuild = do -- this is only sound if modelBuild doesn't do IO.
    ((out1,out2), vars) <- build $ runWriterT modelBuild
    Model _ _ o1 <- buildWriter (return out1 :: WriterBuild a)
    Model _ _ o2 <- buildWriter (return out2 :: WriterBuild b)
    return $ Model [] vars $ o1 ++ o2

instance (Encodable a, BuildMagic r e) => BuildMagic (Input a -> r) (a -> e) where
  buildWriter modelBuild = do
    inp <- variable $ getEncodingShape (Phant :: Phant a)
    vinp <- renderValue $ readValue inp
    Model inps vs out <- buildWriter $ modelBuild vinp
    return $ Model (inp:inps) vs out

--------------------
-- model calculus --
--------------------

applyVal :: (Encodable a) => Model (a -> b) -> a -> (Feed, Model b)
applyVal (Model (i:r) vs outs) a = (feed i $ encodeInTensor a, Model r vs outs)

class ApplyVals a r b | a r -> b where
  applyVals :: Model a -> r -> ([Feed], Model b)
instance ApplyVals a () a where
  applyVals m () = ([], m)
instance (Encodable a, ApplyVals b r c) => ApplyVals (a -> b) (a,r) c where
  applyVals (Model (i:r) vs outs) (a,rest) = (feed i (encodeInTensor a):fd, m)
    where (fd, m) = applyVals (Model r vs outs :: Model b) rest

returnOutput :: Tensor v Float -> WriterBuild (Output a)
returnOutput = renderValue

--------------------
-- New Parameters --
--------------------
initializedParam :: FTensor v -> WriterBuild FNode
initializedParam initializer = do
  w <- initializedVariable initializer
  tell [w]
  renderValue $ readValue w

initializedRandomParam :: Int64 -> Shape -> WriterBuild FNode
initializedRandomParam len shape = initializedParam =<< randomParam len shape


------------------
-- Example code --
------------------
buildModelEx :: forall a m . (Encodable a, MonadBuild m) => m (Model (a -> ()))
buildModelEx = buildWriter $ \(input_one :: Input a) -> do -- should get converted automatically into placeholders.
  returnOutput undefined
