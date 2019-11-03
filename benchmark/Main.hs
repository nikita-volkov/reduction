module Main where

import Prelude
import Criterion
import Criterion.Main
import Reduction (Reduction)
import Control.Foldl (Fold)
import qualified Reduction
import qualified Control.Foldl as Foldl
import qualified Data.Vector


main = defaultMain
  [
    bgroup "Sum" $ let
      input = [0..999999] :: [Int]
      in [
          bench "Reduction" $ nf (reduceList Reduction.sum) input
          ,
          bench "Reduction with early termination" $ let
            reduction = Reduction.onTaken 99999 Reduction.sum
            in nf (reduceList reduction) input
          ,
          bench "Foldl" $ nf (Foldl.fold Foldl.sum) input
          ,
          bench "foldl' (+) 0" $ nf (foldl' (+) 0) input
          ,
          bench "sum" $ nf sum input
        ]
    ,
    bgroup "vector" $ let
      input = [0..999999] :: [Int]
      in [
          bench "Reduction" $ nf (reduceList (Reduction.vector :: Reduction Int (Data.Vector.Vector Int))) input
          ,
          bench "Foldl" $ nf (Foldl.fold (Foldl.vector :: Fold Int (Data.Vector.Vector Int))) input
        ]
    ,
    bgroup "Parallel composition" $ let
      input = [0..999999] :: [Int]
      in [
          bench "Reduction" $ let
            reduction :: Reduction Int (Int, Int)
            reduction = liftA2 (,) Reduction.sum Reduction.count
            in nf (reduceList reduction) input
          ,
          bench "Foldl" $ let
            fold = liftA2 (,) Foldl.sum Foldl.length
            in nf (Foldl.fold fold) input
        ]
    ,
    bgroup "null (early termination)" $ let
      input = [0..999] :: [Int]
      in [
          bench "Reduction" $ nf (reduceList Reduction.null) input
          ,
          bench "Foldl" $ nf (Foldl.fold Foldl.null) input
        ]
    ,
    bench "Text decoding" $ let
      !input = concat $ replicate 10000 $ ["\208\176\208", "\177\208\178\208\179\208", "\180\208\181\209\145\208\182\208\183", "\208\184\208\185\208\186\208\187\208\188\208\189\208\190\208\191\209", "\128\209\129\209\130\209\136\209\137\209\140\209\138\209\141\209\142\209\143"]
      in nf (reduceList Reduction.decodeUtf8) input
  ]

reduceList :: Reduction a b -> [a] -> b
reduceList reduction list = Reduction.extract (Reduction.feedList list reduction)
