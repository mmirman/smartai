{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
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

data Model = Model { modelInputs    :: [FVar]
                   , modelVariables :: [FVar]
                   , modelOutputs   :: [FNode]
                   }

newtype MModel a = MModel { unMModel :: forall m . MonadBuild m => m Model }

mmodel :: Model -> MModel a
mmodel a = MModel $ return a

data Phant a = Phant

class Encodable a where
  getEncodingShape :: Phant a -> Shape
  encodeInTensor :: a -> TensorData Float
  
class BuildMagic i o | i -> o where
  buildWriter :: i -> MModel o

instance BuildMagic (WriterBuild (Output a)) a where
  buildWriter modelBuild = MModel $ do
    (out, vars) <- build $ runWriterT modelBuild
    return $ Model [] vars [out]

instance BuildMagic (WriterBuild [Output a]) [a] where
  buildWriter modelBuild = MModel $ do
    (out, vars) <- build $ runWriterT modelBuild
    return $ Model [] vars out

instance (BuildMagic (WriterBuild a) a', BuildMagic (WriterBuild b) b') => BuildMagic (WriterBuild (a, b)) (a',b') where
  buildWriter modelBuild = MModel $ do -- this is only sound if modelBuild doesn't do IO.
    ((out1,out2), vars) <- build $ runWriterT modelBuild
    Model _ _ o1 <- unMModel $ buildWriter (return out1 :: WriterBuild a)
    Model _ _ o2 <- unMModel $ buildWriter (return out2 :: WriterBuild b)
    return $ Model [] vars $ o1 ++ o2

instance (Encodable a, BuildMagic r e) => BuildMagic (Input a -> r) (a -> e) where
  buildWriter modelBuild = MModel $ do
    inp <- variable $ getEncodingShape (Phant :: Phant a)
    vinp <- renderValue $ readValue inp
    Model inps vs out <- unMModel $ buildWriter $ modelBuild vinp
    return $ Model (inp:inps) vs out


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

--------------------
-- Model Calculus --
--------------------
-- the result can be destroyed by using the second model elsewhere!!
compose :: Encodable b => MModel (a -> b) -> MModel (b -> c) -> MModel (a -> c)
compose m1 m2 = MModel $ do
  Model ins vars outs <- unMModel m1
  Model ins' vars' outs' <- unMModel m2
  sequence_ $ zipWith assign ins' outs
  return $ Model ins (vars ++ vars') outs'

apply :: Encodable a => MModel a -> MModel (a -> b) ->  MModel b
apply m1 m2 = MModel $ do
  Model [] vars outs <- unMModel m1
  Model ins' vars' outs' <- unMModel m2
  sequence_ $ zipWith assign ins' outs
  return $ Model [] (vars ++ vars') outs'

-- freeze the first module for every use in the second.
-- can be used for convolutions and stuff.  
letIn :: MModel a -> (MModel a -> MModel b) -> MModel b
letIn mm1 fm2 = MModel $ do
  Model i1 v1 o1 <- unMModel mm1
  Model i2 v2 o2 <- unMModel $ fm2 $ mmodel $ Model i1 [] o1
  return $ Model i2 (v1 ++ v2) o2




---------------------
-- Training Models --
---------------------
applyVal :: (Encodable a, MonadBuild m) => MModel (a -> b) -> a -> m (Feed, MModel b)
applyVal m1 a = do
  Model (i:r) vs outs <- unMModel m1
  return (feed i $ encodeInTensor a, mmodel $ Model r vs outs)

class ApplyVals a r b | a r -> b where
  applyVals :: MonadBuild m => MModel a -> r -> m ([Feed], MModel b)
instance ApplyVals a () a where
  applyVals m () = return ([], m)
instance (Encodable a, ApplyVals b r c) => ApplyVals (a -> b) (a,r) c where
  applyVals m (a,rest) = do
    Model (i:r) vs outs <- unMModel m
    (fd, m) <- applyVals (mmodel $ Model r vs outs :: MModel b) rest    
    return (feed i (encodeInTensor a):fd, m)

simpleTrainModel :: (ApplyVals a input output, Encodable a, Encodable output, Encodable label)
                    => MModel a  -- the model which takes "inputs" and outputs "outputs"
                    -> MModel (output -> label -> Float) -- the loss model 
                    -> (forall m . MonadBuild m => FNode -> [Variable Float] -> m ControlNode)
                    -> input -> label -> Session ()
simpleTrainModel totrain loss_model minimizer ins label = do
  (feeds, mo) <- applyVals totrain ins
  (feed, final_loss) <- applyVal (apply mo loss_model) label
  Model [] vars [out] <- unMModel final_loss
  trainStep <- minimizer out vars
  runWithFeeds_ (feed:feeds) trainStep

------------------
-- Example code --
------------------
buildModelEx :: forall a m . Encodable a => MModel (a -> ())
buildModelEx = buildWriter $ \(input_one :: Input a) -> do -- should get converted automatically into placeholders.
  returnOutput undefined
