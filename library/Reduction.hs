module Reduction
(
  -- * Reduction
  Reduction(..),
  -- ** Execution
  extract,
  -- ** Construction
  foldl,
  concat,
  count,
  sum,
  product,
  list,
  reverseList,
  -- *** Attoparsec integration
  parseText,
  parseByteString,
  -- ** Transformation
  take,
  drop,
  takeWhile,
  dropWhile,
  -- *** Attoparsec integration
  parseTextStream,
  parseByteStringStream,
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
  feedFoldable,
  -- ** Compositional conversion
  unpar,
  unseq,
  -- * Parallel reduction
  ParReduction,
  par,
  -- * Sequential reduction
  SeqReduction,
  seq,
)
where

import Reduction.Prelude hiding (par, seq, foldl, sum, product, take, drop, concat, takeWhile, dropWhile)
import qualified Data.Vector.Generic as Vec
import qualified Control.Comonad as Comonad
import qualified Data.Attoparsec.Types as Atto
import qualified Data.Attoparsec.Text as AttoText
import qualified Data.Attoparsec.ByteString as AttoByteString
import qualified Reduction.String as String
import qualified Data.Text as Text


-- * Reduction
-------------------------

{-|
State of reduction.
A powerful replacement to folding functions,
with the following features:

- Feeding from multiple sources of data.
- Early termination. Stops consuming input as soon as it stops needing it.
- Being itself a state value allows it to be fed with data incrementally or serve as context in the `State` monad.
- Composes well. Providing both parallel and sequential APIs.
-}
data Reduction input output =
  {-|
  State in which it is ready to either terminate or process more input.
  -}
  Ongoing
    output {-^ Terminate. -}
    !(input -> Reduction input output) {-^ Get the next reduction state by processing more input. -} |
  {-|
  State in which it is no longer ready to process more input.
  -}
  Terminated !output

deriving instance Functor (Reduction input)

instance Comonad.Comonad (Reduction input) where
  extract = extract
  duplicate = \ case
    Ongoing terminate consume -> Ongoing (Ongoing terminate consume) (Comonad.duplicate . consume)
    Terminated output -> Terminated (Terminated output)

instance Profunctor Reduction where
  dimap proj1 proj2 = \ case
    Ongoing terminate consume -> Ongoing (proj2 terminate) (dimap proj1 proj2 . consume . proj1)
    Terminated output -> Terminated (proj2 output)

instance Choice Reduction where
  right' = \ case
    Ongoing terminate consume ->
      Ongoing (Right terminate) $ \ case
        Left o -> Terminated (Left o)
        Right i -> right' (consume i)
    Terminated o -> Terminated (Right o)

-- ** Execution
-------------------------

{-|
Extract the current result of reduction. Same as @Control.Comonad.`Comonad.extract`@.

Use this function in combination with ones of the @feed*@ family to reduce datastructures.
E.g.,

>>> sum & feedList [1,2,3] & extract
6
-}
{-# INLINABLE extract #-}
extract :: Reduction input output -> output
extract = \ case
  Ongoing terminate _ -> terminate
  Terminated output -> output

-- ** Construction
-------------------------

{-|
Reduction, performing strict left fold.
-}
{-# INLINABLE foldl #-}
foldl :: (b -> a -> b) -> b -> Reduction a b
foldl step = let
  fromState !state = Ongoing state (fromState . step state)
  in fromState

{-|
Concatenate monoid values.
-}
{-# INLINABLE concat #-}
concat :: Monoid a => Reduction a a
concat = foldl mappend mempty

{-|
Reduction, counting the visited elements.
-}
{-# INLINABLE count #-}
count :: Num count => Reduction a count
count = let
  counted !n = Ongoing n (const (counted (n + 1)))
  in counted 0

{-|
Reduction, summarizing all visited elements.
-}
{-# INLINABLE sum #-}
sum :: Num a => Reduction a a
sum = foldl (+) 0

{-|
Reduction, multiplying all visited elements.
-}
{-# INLINABLE product #-}
product :: Num a => Reduction a a
product = foldl (*) 1

{-|
Reduction, collecting all visited elements into a list.
It's slower than `reverseList`.

-}
{-# INLINABLE list #-}
list :: Reduction a [a]
list = fmap reverse reverseList

{-|
Reduction, collecting all visited elements into a list in reverse order.
It's faster than `list`.
-}
{-# INLINABLE reverseList #-}
reverseList :: Reduction a [a]
reverseList = foldl (flip (:)) []

-- *** Attoparsec
-------------------------

{-|
Convert an Attoparsec text parser into reduction over text chunks.

>>> Data.Attoparsec.Text.decimal & parseText & feed "123" & feed "45" & extract
Right 12345
-}
{-# INLINABLE parseText #-}
parseText :: AttoText.Parser o -> Reduction Text (Either String o)
parseText parser =
  Ongoing
    (extract (parserResult (AttoText.parse parser "")))
    (parserResult . AttoText.parse parser)

{-|
Convert an Attoparsec bytestring parser into reduction bytestring chunks.
-}
{-# INLINABLE parseByteString #-}
parseByteString :: AttoByteString.Parser o -> Reduction ByteString (Either String o)
parseByteString parser =
  Ongoing
    (extract (parserResult (AttoByteString.parse parser "")))
    (parserResult . AttoByteString.parse parser)

{-# INLINABLE parserResult #-}
parserResult :: Monoid i => Atto.IResult i o -> Reduction i (Either String o)
parserResult = let
  terminateCont cont = case cont mempty of
    Atto.Done _ o -> Right o
    Atto.Fail _ context details -> Left (String.attoFailure context details)
    _ -> Left "Result: incomplete input"
  in \ case
    Atto.Partial cont -> Ongoing (terminateCont cont) (parserResult . cont)
    Atto.Done _ o -> Terminated (Right o)
    Atto.Fail _ context details -> Terminated (Left (String.attoFailure context details))

-- ** Transformation
-------------------------

{-# INLINABLE forceTermination #-}
forceTermination :: Reduction a b -> Reduction a b
forceTermination = \ case
  Ongoing terminate _ -> Terminated terminate
  terminated -> terminated

{-# INLINABLE mapOngoing #-}
mapOngoing :: (Reduction a b -> Reduction a b) -> Reduction a b -> Reduction a b
mapOngoing fn = \ case
  Ongoing terminate consume -> Ongoing terminate (fn . consume)
  terminated -> terminated

{-|
Limit a reduction to consume only the specified amount of elements at max,
terminating early.

>>> list & take 2 & feedList [1,2,3,4] & extract
[1,2]
-}
{-# INLINABLE take #-}
take :: Int -> Reduction a b -> Reduction a b
take amount = if amount > 0
  then mapOngoing (take (pred amount))
  else forceTermination

{-|
Make reduction ignore the first elements.

>>> list & drop 2 & feedList [1,2,3,4] & extract
[3,4]
-}
{-# INLINABLE drop #-}
drop :: Int -> Reduction a b -> Reduction a b
drop amount = if amount > 0
  then \ reduction -> case reduction of
    Ongoing terminate consume -> Ongoing terminate (const (drop (amount - 1) reduction))
    _ -> reduction
  else id

{-|
>>> list & takeWhile (< 3) & feedList [1,2,3,4] & extract
[1,2]
-}
{-# INLINABLE takeWhile #-}
takeWhile :: (a -> Bool) -> Reduction a b -> Reduction a b
takeWhile predicate = let
  loop = \ case
    Ongoing terminate consume ->
      Ongoing terminate $ \ input -> if predicate input
        then loop (consume input)
        else Terminated terminate
    Terminated output -> Terminated output
  in loop

{-|
>>> list & dropWhile (< 3) & feedList [1,2,3,4] & extract
[3,4]
-}
{-# INLINABLE dropWhile #-}
dropWhile :: (a -> Bool) -> Reduction a b -> Reduction a b
dropWhile predicate = let
  loop = \ case
    Ongoing terminate consume ->
      Ongoing terminate $ \ input -> if predicate input
        then loop (Ongoing terminate consume)
        else consume input
    Terminated output -> Terminated output
  in loop

-- *** Attoparsec integration
-------------------------

{-|
Parse a stream of values, reducing it to a final result.

>>> :{
  let
    parser = Data.Attoparsec.Text.decimal <* Data.Attoparsec.Text.char ','
    in list & parseTextStream parser & feedList ["12,", "3", ",4,"] & extract
:}
Right [12,3,4]
-}
{-# INLINABLE parseTextStream #-}
parseTextStream :: AttoText.Parser a -> Reduction a b -> Reduction Text (Either String b)
parseTextStream parser = let
  handleResult = \ case
    Atto.Partial cont -> \ case
      Ongoing terminate consume ->
        Ongoing
          (Right terminate)
          (\ chunk -> handleResult (cont chunk) (Ongoing terminate consume))
      Terminated output -> Terminated (Right output)
    Atto.Done remainderInput value -> \ case
      Ongoing _ consume ->
        handleResult
          (AttoText.parse parser remainderInput)
          (consume value)
      Terminated output -> Terminated (Right output)
    Atto.Fail _ context details -> const (Terminated (Left (String.attoFailure context details)))
  in handleResult (AttoText.parse parser "")

{-|
Parse a stream of values, reducing it to a final result.
-}
{-# INLINABLE parseByteStringStream #-}
parseByteStringStream :: AttoByteString.Parser a -> Reduction a b -> Reduction ByteString (Either String b)
parseByteStringStream parser = let
  handleResult = \ case
    Atto.Partial cont -> \ case
      Ongoing terminate consume ->
        Ongoing
          (Right terminate)
          (\ chunk -> handleResult (cont chunk) (Ongoing terminate consume))
      Terminated output -> Terminated (Right output)
    Atto.Done remainderInput value -> \ case
      Ongoing _ consume ->
        handleResult
          (AttoByteString.parse parser remainderInput)
          (consume value)
      Terminated output -> Terminated (Right output)
    Atto.Fail _ context details -> const (Terminated (Left (String.attoFailure context details)))
  in handleResult (AttoByteString.parse parser "")

-- *** Feeding
-------------------------

{-|
Update reduction by feeding one input to it.
-}
{-# INLINABLE feed #-}
feed :: a -> Reduction a output -> Reduction a output
feed input = \ case
  Ongoing _ consume -> consume input
  terminated -> terminated

{-|
Update reduction by feeding a list of inputs to it.
-}
{-# INLINABLE feedList #-}
feedList :: [a] -> Reduction a output -> Reduction a output
feedList = \ case
  input : remainingList -> \ case
    Ongoing _ consume -> consume input & feedList remainingList
    terminated -> terminated
  _ -> id

{-|
Update reduction by feeding a vector of inputs to it.
-}
{-# INLINABLE feedVector #-}
feedVector :: Vector vec input => vec input -> Reduction input output -> Reduction input output
feedVector vec = let
  length = Vec.length vec
  iterate index = \ case
    Ongoing terminate consume -> if index < length
      then iterate (succ index) (consume (Vec.unsafeIndex vec index))
      else Ongoing terminate consume
    terminated -> terminated
  in iterate 0

{-|
Update reduction by feeding a foldable of inputs to it.
-}
{-# INLINABLE feedFoldable #-}
feedFoldable :: Foldable f => f input -> Reduction input output -> Reduction input output
feedFoldable foldable reduction =
  foldr
    (\ input updateReduction reduction -> case reduction of
      Ongoing _ consume -> updateReduction (consume input)
      terminated -> terminated
    )
    id
    foldable
    reduction

-- *** Composition
-------------------------

{-# INLINABLE apPar #-}
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

{-# INLINABLE selectPar #-}
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

{-# INLINABLE altPar #-}
altPar :: Reduction input a -> Reduction input a -> Reduction input a
altPar = \ case
  Ongoing terminate1 consume1 -> \ case
    Ongoing terminate2 consume2 ->
      Ongoing terminate1 (\ i -> altPar (consume1 i) (consume2 i))
    Terminated output2 -> Terminated output2
  Terminated output1 -> const (Terminated output1)

{-# INLINABLE apSeq #-}
apSeq :: Reduction input (a -> b) -> Reduction input a -> Reduction input b
apSeq = \ case
  Ongoing terminate1 consume1 -> \ reduction2 ->
    Ongoing
      (terminate1 (extract reduction2))
      (\ input -> apSeq (consume1 input) reduction2)
  Terminated output1 -> fmap output1

{-# INLINABLE bindSeq #-}
bindSeq :: (a -> Reduction input b) -> Reduction input a -> Reduction input b
bindSeq getReduction2 = 
  let
    compose = \ case
      Ongoing terminate1 consume1 ->
        let
          terminate = extract (getReduction2 terminate1)
          consume input = compose (consume1 input)
          in Ongoing terminate consume
      Terminated output1 -> getReduction2 output1
    in compose

-- ** Compositional conversion
-------------------------

{-|
Normalize parallel reduction.
-}
{-# INLINABLE unpar #-}
unpar :: ParReduction input output -> Reduction input output
unpar (ParReduction reduction) = reduction

{-|
Normalize sequential reduction.
-}
{-# INLINABLE unseq #-}
unseq :: SeqReduction input output -> Reduction input output
unseq (SeqReduction reduction) = reduction


-- * Parallel
-------------------------

{-|
Zero-cost wrapper over `Reduction`,
which provides instances, implementing parallel composition.

>>> :{
  extract $ feedList [1,2,3,4] $ unpar $
    (,) <$>
      par (take 2 list) <*>
      par (take 3 list)
:}
([1,2],[1,2,3])
-}
newtype ParReduction input output = ParReduction (Reduction input output)

deriving instance Functor (ParReduction input)

deriving instance Profunctor ParReduction

deriving instance Choice ParReduction

{-|
Feeds all reductions, combining their results.
-}
instance Applicative (ParReduction input) where
  pure a = ParReduction (Terminated a)
  (<*>) = parBinOp apPar

{-|
Feeds all reductions, terminating early if possible.
-}
instance Selective (ParReduction input) where
  select = parBinOp selectPar

{-|
Feeds all reductions, getting the result of the one that terminates first.
-}
instance Alt (ParReduction input) where
  (<!>) = parBinOp altPar

parBinOp op (ParReduction red1) (ParReduction red2) = ParReduction (op red1 red2)

{-|
Parallelize normal reduction.
-}
{-# INLINABLE par #-}
par :: Reduction input output -> ParReduction input output
par = ParReduction


-- * Sequential
-------------------------

{-|
Zero-cost wrapper over `Reduction`,
which provides instances, implementing sequential composition.

>>> ((,) <$> seq (take 2 list) <*> seq list) & unseq & feedList [1,2,3,4] & extract
([1,2],[3,4])

>>> :{
  extract $ feedList [1,2,3,4] $ unseq $ do
    a <- seq $ take 2 $ sum
    b <- seq $ product
    return (a, b)
:}
(3,12)
-}
newtype SeqReduction input output = SeqReduction (Reduction input output)

deriving instance Functor (SeqReduction input)

deriving instance Profunctor SeqReduction

deriving instance Choice SeqReduction

instance Applicative (SeqReduction input) where
  pure a = SeqReduction (Terminated a)
  (<*>) = seqBinOp apSeq

instance Selective (SeqReduction input) where
  select = selectM

instance Monad (SeqReduction input) where
  return = pure
  (>>=) (SeqReduction reduction1) getSeqReduction2 =
    SeqReduction (bindSeq (unseq . getSeqReduction2) reduction1)

seqBinOp op (SeqReduction red1) (SeqReduction red2) = SeqReduction (op red1 red2)

{-|
Sequentialize normal reduction.
-}
{-# INLINABLE seq #-}
seq :: Reduction input output -> SeqReduction input output
seq = SeqReduction
