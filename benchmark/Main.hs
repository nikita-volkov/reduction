module Main where

import Prelude
import Criterion
import Criterion.Main
import Reduction (Reduction)
import Control.Foldl (Fold)
import qualified Reduction
import qualified Control.Foldl as Foldl


main = defaultMain
  [
    bgroup "Sum" $ let
      input = [0..999999] :: [Int]
      in [
          bench "Reduction" $ nf (reduceList Reduction.sum) input
          ,
          bench "Reduction with early termination" $ let
            reduction = Reduction.take 99999 Reduction.sum
            in nf (reduceList reduction) input
          ,
          bench "Foldl" $ nf (Foldl.fold Foldl.sum) input
          ,
          bench "foldl' (+) 0" $ nf (foldl' (+) 0) input
          ,
          bench "sum" $ nf sum input
        ]
    ,
    bgroup "Parallel composition" $ let
      input = [0..999999] :: [Int]
      in [
          bench "Reduction" $ let
            reduction :: Reduction Int (Int, Int)
            reduction =
              Reduction.unpar $
              liftA2 (,) (Reduction.par Reduction.sum) (Reduction.par Reduction.count)
            in nf (reduceList reduction) input
          ,
          bench "Foldl" $ let
            fold = liftA2 (,) Foldl.sum Foldl.length
            in nf (Foldl.fold fold) input
        ]
  ]

reduceList :: Reduction a b -> [a] -> b
reduceList reduction list = Reduction.extract (Reduction.feedList list reduction)
