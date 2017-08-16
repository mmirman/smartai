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
type FVar = Variable Float

type WriterBuild v = WriterT [FVar] Build v
instance MonadBuild (WriterT [FVar] Build) where
  build m = lift m

initializedRandomParam :: Int64 -> Shape -> WriterBuild (FTensor Build)
initializedRandomParam len shape = do
  w <- initializedVariable =<< randomParam len shape
  tell [w]
  return $ readValue w


type Input v = FTensor Value
type Output v = FTensor Build

data Model i = Model { modelInputs    :: [FTensor Value]
                     , modelVariables :: [FVar]
                     , modelOutputs :: [FTensor Build]
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
    (inp :: FTensor Value) <- placeholder $ getEncodingShape (Phant :: Phant a) 
    Model inps vs out <- buildWriter (modelBuild inp)
    return $ Model (inp:inps) vs out

returnOutput :: Output a -> WriterBuild (Output a)
returnOutput = return

buildModelEx :: forall a m . (Encodable a, MonadBuild m) => m (Model (a -> ()))
buildModelEx = buildWriter $ \(input_one :: Input a) -> do -- should get converted automatically into placeholders.
  returnOutput undefined



compose :: MonadBuild m => Model (a -> b) -> Model (b -> c) -> m (Model (a -> c))
compose = undefined

apply :: MonadBuild m => Model (a -> b) -> Model a -> m (Model b)
apply = undefined


abstract :: forall m a b . (MonadBuild m, Encodable a) => (Input a -> Model b) -> m (Model (a -> b))
abstract foo = do
  (inp :: Input a) <- placeholder $ getEncodingShape (Phant :: Phant a)
  let Model inps vars outs = foo inp
  return $ Model (inp:inps) vars outs


applyVal :: (Encodable a) => Model (a -> b) -> a -> (Feed, Model b)
applyVal (Model (i:r) vs outs) a = (feed i $ encodeInTensor a, Model r vs outs)

class ApplyVals a r b | a r -> b where
  applyVals :: Model a -> r -> ([Feed], Model b)
instance ApplyVals a () a where
  applyVals m () = ([], m)
instance (Encodable a, ApplyVals b r c) => ApplyVals (a -> b) (a,r) c where
  applyVals (Model (i:r) vs outs) (a,rest) = (feed i (encodeInTensor a):fd, m)
    where (fd, m) = applyVals (Model r vs outs :: Model b) rest
    
  
