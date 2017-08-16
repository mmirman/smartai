{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
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

data Data
data Func

data Model t i where
  Functional ::  [FTensor Value] -> [FVar] ->  [FTensor Build] -> Model Func (a -> b)
  Data :: [FVar] -> [FTensor Build] -> Model Data i

getModelOuts :: Model t i -> [Output i]
getModelOuts (Functional _ _ outs) = outs
getModelOuts (Data _ outs) = outs

getModelIns :: Model t i -> [Input i]
getModelIns (Functional ins _ _) = ins
getModelIns (Data _ _) = []

getModelWeights :: Model t i -> [FVar]
getModelWeights (Functional _ w _) = w
getModelWeights (Data w _) = w

class BuildMagic i o t | i -> o t where
  buildWriter :: MonadBuild m => i -> m (Model t o)

instance BuildMagic (WriterBuild (Output a)) a Data where
  buildWriter modelBuild = do
    (out, vars) <- build $ runWriterT modelBuild
    return $ Data vars [out]

instance BuildMagic (WriterBuild [Output a]) [a] Data where
  buildWriter modelBuild = do
    (out, vars) <- build $ runWriterT modelBuild
    return $ Data vars out

instance (BuildMagic (WriterBuild a) a' Data, BuildMagic (WriterBuild b) b' Data) => BuildMagic (WriterBuild (a, b)) (a',b') Data where
  buildWriter modelBuild = do -- this is only sound if modelBuild doesn't do IO.
    ((out1,out2), vars) <- build $ runWriterT modelBuild
    m1 <- buildWriter (return out1 :: WriterBuild a)
    m2 <- buildWriter (return out2 :: WriterBuild b)
    return $ Data vars (getModelOuts m1 ++ getModelOuts m2)
    

instance BuildMagic r e t => BuildMagic (Input a -> r) (a -> e) Func where
  buildWriter modelBuild = do
    (inp :: FTensor Value) <- placeholder []
    m <- buildWriter (modelBuild inp)
    return $ Functional (inp:getModelIns m) (getModelWeights m) (getModelOuts m)

returnOutput :: Output a -> WriterBuild (Output a)
returnOutput = return

buildModelEx :: forall a m . MonadBuild m => m (Model Func (a -> ()))
buildModelEx = buildWriter $ \(input_one :: Input a) -> do -- should get converted automatically into placeholders.
  returnOutput undefined



compose :: Model Func (a -> b) -> Model Func (b -> c) -> Model Func (a -> c)
compose = undefined

--apply :: Model (a -> b) -> Model a -> Model (a -> c)
