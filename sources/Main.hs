{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad.Writer.Lazy

import Data.Foldable (forM_)
import Data.Vector (Vector)
import Control.Monad (replicateM, replicateM_)
import System.Random (randomIO)
import Test.HUnit (assertBool)

import SPrelude as S
import Prelude as P

type WriterBuild a v = WriterT [Variable a] Build v
instance MonadBuild (WriterT [a] Build) where
  build m = lift m

initializedRandomParam :: Int64 -> Shape -> WriterBuild Float (Tensor Build Float)
initializedRandomParam len shape = do
  w <- initializedVariable =<< randomParam len shape
  tell [w]
  return $ readValue w


data Model a = Model { modelTrainer :: forall t . Fetchable t a => t -> Session a
                     , modelVariables :: [Variable a]
                     }
               
buildWriter :: MonadBuild m => ([TensorData a] -> WriterBuild a ([Tensor Value a], Tensor Build a)) -> [TensorData a] -> m (Model a)
buildWriter modelBuild datr = fmap (\((inputs, loss),vars) -> Model (runWithFeeds $ zipWith feed inputs datr) vars) $ build $ runWriterT $ modelBuild datr

--buildModel :: MonadBuild m => [TensorData a] -> m (Model Float)
buildModel :: MonadBuild m => [TensorData Float] -> m (Model Float)
buildModel = buildWriter $ \idata -> do
    let len = 10 -- castInt $ length $ fst $ head idata
    (inp :: Tensor Value Float) <- placeholder [len]
    (out :: Tensor Value Float) <- placeholder [1]

    w <- initializedRandomParam len [len]

    let loss = inp `matMul` w
        
    return ([inp,out],loss)
        
--tinymodel :: [([Float],Bool)] -> IO (Vector Float)
tinymodel idata = runSession $ do
  Model loss weights <- buildModel idata
    
  --forM_ idata $ \xdata -> do
  --trainStep <- minimizeWith (gradientDescent 0.001) loss weights
  --  run trainStep

  model <- run $ readValue $ head weights
  return model


main :: IO ()
main = do
    -- Generate data where `y = x*3 + 8`.
    xData <- replicateM 100 randomIO
    let yData = [x*3 + 8 | x <- xData]
    -- Fit linear regression model.
    (w, b) <- fit xData yData
    assertBool "w == 3" $ P.abs (3 - w) < 0.001
    assertBool "b == 8" $ P.abs (8 - b) < 0.001


fit :: [Float] -> [Float] -> IO (Float, Float)
fit xData yData = runSession $ do
  
    -- Create tensorflow constants for x and y.
    let x = vector xData
        y = vector yData
        
    -- Create scalar variables for slope and intercept.
    w <- initializedVariable 0
    b <- initializedVariable 0
    
    -- Define the loss function.
    let yHat = (x `mul` readValue w) `add` readValue b
        loss = square $ yHat `sub` y
        
    -- Optimize with gradient descent.
    trainStep <- minimizeWith (gradientDescent 0.001) loss [w, b]
    replicateM_ 1000 $ run trainStep
    
    -- Return the learned parameters.
    (Scalar w', Scalar b') <- run (readValue w, readValue b)
    
    return (w', b')
