{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module SPrelude
       ( module TF
       , castInt
       , randomParam
       )
       where

--import Prelude as TF hiding (round, mod, floor, div, abs, tanh, tan, sqrt, sin, log, exp, cos, atan, asin, acos, sum, minimum, maximum, any, all, min, max, const, concat, writeFile, readFile, print, reverse)
import TensorFlow.Core as TF

import TensorFlow.Minimize as TF
import TensorFlow.Variable as TF
import TensorFlow.Core as TF
import TensorFlow.Tensor as TF
import TensorFlow.Ops as TF hiding (assign, assign', variable, variable', initializedVariable, initializedVariable', zeroInitializedVariable, zeroInitializedVariable', abs, concat, sum)
import Data.Int as TF

import TensorFlow.GenOps.Core as TF hiding ( variable, variable'
                                           , truncatedNormal, truncatedNormal'
                                           , shape, shape'
                                           , save
                                           , restore
                                           , resourceApplyAdam, resourceApplyAdam'
                                           , placeholder, placeholder'
                                           , expandDims, expandDims'
                                           , assignAdd, assignAdd'
                                           , assign, assign'
                                           , noOp
                                           )

import Prelude as P

castInt :: (Integral a, Num b) => a -> b
castInt = fromInteger . toInteger


randomParam :: MonadBuild m => Int64 -> Shape -> m (Tensor Build Float)
randomParam width (Shape shape) = build $ (`mul` stddev) <$> truncatedNormal (vector shape)
  where stddev = scalar $ 1 / P.sqrt (fromIntegral width)
