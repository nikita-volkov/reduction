module Reduction
(
  -- * Reduction
  Reduction(..),
  -- ** Execution
  extract,
  -- ** Construction
  foldl,
  count,
  sum,
  list,
  reverseList,
  -- ** Transformation
  -- *** Feeding
  -- |
  -- Utilities allowing you to update a reduction
  -- by feeding the contents of a datastructure to it.
  -- 
  -- Notice that you can feed multiple times and from different data-structures:
  -- 
  -- >>> list & feedList [1,2] & feed 3 & feedVector (Data.Vector.fromList [4,5]) & extract
  -- [1,2,3,4,5]
  feed,
  feedList,
  feedVector,
  -- ** Compositional conversion
  unpar,
  -- * Parallel reduction
  ParReduction,
  par,
)
where

import Reduction.Prelude hiding (par, seq, foldl, sum, product)
import qualified Data.Vector.Generic as Vec
import qualified Control.Comonad as Comonad


-- * Reduction
-------------------------

data Reduction input output =
  Ongoing output !(input -> Reduction input output) |
  Terminated !output

deriving instance Functor (Reduction input)

instance Comonad.Comonad (Reduction input) where
  extract = extract
  duplicate = \ case
    Ongoing terminate consume -> Ongoing (Ongoing terminate consume) (Comonad.duplicate . consume)
    Terminated output -> Terminated (Terminated output)

-- ** Execution
-------------------------

{-|
Extract the current result of reduction. Same as @Control.Comonad.`Comonad.extract`@.

Use this function in combination with ones of the @feed*@ family to reduce datastructures.
E.g.,

>>> sum & feedList [1,2,3] & extract
6
-}
extract :: Reduction input output -> output
extract = \ case
  Ongoing terminate _ -> terminate
  Terminated output -> output

-- ** Construction
-------------------------

{-|
Reduction, performing strict left fold.
-}
foldl :: (b -> a -> b) -> b -> Reduction a b
foldl step = let
  fromState !state = Ongoing state (fromState . step state)
  in fromState

{-|
Reduction, counting the visited elements.
-}
count :: Num count => Reduction a count
count = let
  counted !n = Ongoing n (const (counted (n + 1)))
  in counted 0

{-|
Reduction, summarizing all visited elements.
-}
sum :: Num a => Reduction a a
sum = foldl (+) 0

{-|
Reduction, collecting all visited elements into a list.
It's slower than `reverseList`.

-}
list :: Reduction a [a]
list = fmap reverse reverseList

{-|
Reduction, collecting all visited elements into a list in reverse order.
It's faster than `list`.
-}
reverseList :: Reduction a [a]
reverseList = foldl (flip (:)) []

-- ** Transformation
-------------------------

-- *** Feeding
-------------------------

{-|
Update reduction by feeding one input to it.
-}
feed :: a -> Reduction a output -> Reduction a output
feed input = \ case
  Ongoing _ consume -> consume input
  terminated -> terminated

{-|
Update reduction by feeding a list of inputs to it.
-}
feedList :: [a] -> Reduction a output -> Reduction a output
feedList = \ case
  input : remainingList -> \ case
    Ongoing _ consume -> consume input & feedList remainingList
    terminated -> terminated
  _ -> id

{-|
Update reduction by feeding a vector of inputs to it.
-}
feedVector :: Vector vec input => vec input -> Reduction input output -> Reduction input output
feedVector vec = let
  length = Vec.length vec
  iterate index = \ case
    Ongoing terminate consume -> if index < length
      then iterate (succ index) (consume (Vec.unsafeIndex vec index))
      else Ongoing terminate consume
    terminated -> terminated
  in iterate 0

-- *** Composition
-------------------------

apPar :: Reduction input (a -> b) -> Reduction input a -> Reduction input b
apPar = \ case
  Ongoing terminate1 consume1 -> \ case
    Ongoing terminate2 consume2 -> let
      terminate = terminate1 terminate2
      consume input = let
        nextReduction1 = consume1 input
        nextReduction2 = consume2 input
        in apPar nextReduction1 nextReduction2
      in Ongoing terminate consume
    Terminated output2 -> let
      terminate = terminate1 output2
      consume = fmap (\ output1 -> output1 output2) . consume1
      in Ongoing terminate consume
  Terminated output1 -> fmap output1

selectPar :: Reduction input (Either a b) -> Reduction input (a -> b) -> Reduction input b
selectPar = \ case
  Ongoing terminate1 consume1 -> \ case
    Ongoing terminate2 consume2 ->
      Ongoing
        (either terminate2 id terminate1)
        (\ input -> selectPar (consume1 input) (consume2 input))
    Terminated output2 ->
      Ongoing
        (either output2 id terminate1)
        (fmap (either output2 id) . consume1)
  Terminated output1 -> case output1 of
    Left output1 -> fmap ($ output1)
    Right output -> const (Terminated output)


-- ** Compositional conversion
-------------------------

{-|
Normalize parallel reduction.
-}
unpar :: ParReduction input output -> Reduction input output
unpar (ParReduction reduction) = reduction


-- * Parallel
-------------------------

{-|
Zero-cost wrapper over `Reduction`,
which provides instances, implementing parallel composition.
-}
newtype ParReduction input output = ParReduction (Reduction input output)

deriving instance Functor (ParReduction input)

instance Applicative (ParReduction input) where
  pure a = ParReduction (Terminated a)
  (<*>) = parBinOp apPar

instance Selective (ParReduction input) where
  select = parBinOp selectPar

parBinOp op (ParReduction red1) (ParReduction red2) = ParReduction (op red1 red2)

{-|
Parallelize normal reduction.
-}
par :: Reduction input output -> ParReduction input output
par = ParReduction
