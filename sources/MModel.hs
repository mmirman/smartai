{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
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

data Phant a = Phant
class Encodable a where
  encodeInTensor :: a -> TensorData Float
  decodeFromTensor :: TensorData Float -> a
  getEncodingName :: Phant a -> String
  
class (MonadBuild m, Monad m) => Modeler m where
  getEncodingShape :: Encodable a => Phant a -> m Shape
  
type Input v = FNode
type Output v = FNode

data Model a = Model { modelInputs    :: [FVar]
                     , modelVariables :: [FVar]
                     , modelOutputs   :: [FNode]
                     }

newtype AbModel a = AbModel { renderModel :: forall m . Modeler m => m (Model a) }

abstractModel :: Model a -> AbModel a
abstractModel m = AbModel $ return m

  
class BuildMagic i o | i -> o where
  buildWriter :: i -> AbModel o

instance BuildMagic (WriterBuild (Output a)) a where
  buildWriter modelBuild = AbModel $ do
    (out, vars) <- build $ runWriterT modelBuild
    return $ Model [] vars [out]

instance BuildMagic (WriterBuild [Output a]) [a] where
  buildWriter modelBuild = AbModel $ do
    (out, vars) <- build $ runWriterT modelBuild
    return $ Model [] vars out

instance (BuildMagic (WriterBuild a) a', BuildMagic (WriterBuild b) b') => BuildMagic (WriterBuild (a, b)) (a',b') where
  buildWriter modelBuild = AbModel $ do -- this is only sound if modelBuild doesn't do IO.
    ((out1,out2), vars) <- build $ runWriterT modelBuild
    Model _ _ o1 <- renderModel $ buildWriter (return out1 :: WriterBuild a)
    Model _ _ o2 <- renderModel $ buildWriter (return out2 :: WriterBuild b)
    return $ Model [] vars $ o1 ++ o2

instance (Encodable a, BuildMagic r e) => BuildMagic (Input a -> r) (a -> e) where
  buildWriter modelBuild = AbModel $ do
    inp <- variable =<< getEncodingShape (Phant :: Phant a)
    vinp <- renderValue $ readValue inp
    Model inps vs out <- renderModel $ buildWriter $ modelBuild vinp
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
compose :: (Encodable b, MonadBuild m) => Model (a -> b) -> Model (b -> c) -> m (Model (a -> c))
compose (Model ins vars outs) (Model ins' vars' outs') = do
  sequence_ $ zipWith assign ins' outs
  return $ Model ins (vars ++ vars') outs'
  
apply :: (Encodable a, MonadBuild m) => Model a -> Model (a -> b) ->  m (Model b)
apply (Model [] vars outs) (Model ins' vars' outs') = do
  sequence_ $ zipWith assign ins' outs
  return $ Model [] (vars ++ vars') outs'
  
-- freeze the first module for every use in the second.
-- can be used for convolutions and stuff.  
letIn :: Model a -> (Model a -> AbModel b) -> AbModel b
letIn (Model i1 v1 o1) fm2 = AbModel $ do
  Model i2 v2 o2 <- renderModel $ fm2 $ Model i1 [] o1
  return $ Model i2 (v1 ++ v2) o2

---------------------
-- Training Models --
---------------------

applyVal :: Encodable a => Model (a -> b) -> a -> (Feed, Model b)
applyVal (Model (i:r) vs outs) a = (feed i $ encodeInTensor a, Model r vs outs)

class ApplyVals a r b | a r -> b where
  applyVals :: Model a -> r -> ([Feed], Model b)
instance ApplyVals a () a where
  applyVals m () = ([], m)
instance (Encodable a, ApplyVals b r c) => ApplyVals (a -> b) (a,r) c where
  applyVals (Model (i:r) vs outs) (a,rest) = (feed i (encodeInTensor a):fd, m)
    where (fd, m) = applyVals (Model r vs outs :: Model b) rest    


simpleTrainModel :: (ApplyVals a input output, Encodable a, Encodable output, Encodable label)
                    => Model a  -- the model which takes "inputs" and outputs "outputs"
                    -> Model (output -> label -> Float) -- the loss model 
                    -> (forall m . MonadBuild m => FNode -> [Variable Float] -> m ControlNode)
                    -> input -> label -> Session ()
simpleTrainModel totrain loss_model minimizer ins label = do
  let (feeds, mo) = applyVals totrain ins
  ml <- apply mo loss_model
  let (feed, Model [] vars [out]) = applyVal ml label
  trainStep <- minimizer out vars
  runWithFeeds_ (feed:feeds) trainStep

simpleUseModel :: (ApplyVals a input output, Encodable a, Encodable output)
                  => Model a  -- the model which takes "inputs" and outputs "outputs"
                  -> input
                  -> Session output
simpleUseModel totrain ins = do
  let (feeds, Model [] vars [out]) = applyVals totrain ins
  runWithFeeds feeds out

------------------
-- Example code --
------------------

data Image
instance Encodable Image where
  --getEncodingShape _ = return [10, 1000] -- [batchSize, numPixels]
  encodeInTensor a = undefined
  getEncodingName _ = "Image"
  
instance Modeler (WriterT [FVar] Build) where
  getEncodingShape a = case getEncodingName a of
    "Image" -> return [3, 1000]

buildModelEx :: forall a m . Encodable a => AbModel (a -> ())
-- should get converted automatically into placeholders.
buildModelEx = buildWriter $ \(input_one :: Input a) -> do
  returnOutput undefined
