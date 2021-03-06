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
  null,
  sum,
  product,
  head,
  find,
  list,
  reverseList,
  strictList,
  reverseStrictList,
  vector,
  hashMap,
  decodeUtf8,
  -- *** Attoparsec integration
  parseText,
  parseByteString,
  -- ** Transformation
  onTaken,
  onDropped,
  onTakenWhile,
  onDroppedWhile,
  onPartitions,
  onEither,
  onByteStringBytes,
  onTextChars,
  onUtf8DecodedText,
  onUnique,
  onFiltered,
  -- *** Attoparsec integration
  onParsedText,
  onParsedByteString,
  -- *** Feeding
  -- |
  -- Utilities allowing you to update a reduction
  -- by feeding the contents of a datastructure to it.
  -- 
  -- Notice that you can feed multiple times and from different datastructures:
  -- 
  -- >>> list & feedList [1,2] & feed 3 & feedVector (Data.Vector.fromList [4,5]) & extract
  -- [1,2,3,4,5]
  feed,
  feedList,
  feedStrictList,
  feedVector,
  feedByteString,
  feedText,
  feedFoldable,
  -- * Recipies
  -- ** List API simulation
  {-|
  The focusing transducers can be applied to many standard problems in interesting ways.
  E.g., here's how you can implement @Data.List.`find`@:

  >>> :{
    let
      find :: (a -> Bool) -> [a] -> Maybe a
      find predicate input = head & onFiltered predicate & feedList input & extract
      in find (> 3) [1,2,3,4,5,6]
  :}
  Just 4

  Due to support for early termination you can be sure that this function will stop
  traversing the list as soon as it finds the element.
  -}
  -- ** Math composition
  {-|
  While it does seem a bit unusual,
  you can apply math functions directly to reductions.

  E.g., here's how you can implement an averaging reduction:

  >>> :{
    let
      avg :: Reduction Double Double
      avg = sum / count
      in avg & feedList [3,2,3,2] & extract
  :}
  2.5
  -}
)
where

import Reduction.Prelude hiding (par, seq, foldl, sum, product, take, drop, concat, takeWhile, dropWhile, either, null, head, find)
import qualified Reduction.Prelude as Prelude
import qualified Data.Vector.Generic as Vec
import qualified Control.Comonad as Comonad
import qualified Data.Attoparsec.Types as Atto
import qualified Data.Attoparsec.Text as AttoText
import qualified Data.Attoparsec.ByteString as AttoByteString
import qualified Reduction.Text as Text
import qualified Reduction.Vector as Vector
import qualified Data.Text as Text
import qualified Data.Text.Unsafe as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified StrictList
import qualified Text.Builder as TextBuilder


-- * Reduction
-------------------------

{-|
State of reduction of multiple @input@ values into one @output@.
A powerful replacement to folding functions,
with the following features:

- Feeding from multiple sources of data.
- Early termination. Stops consuming input as soon as it stops needing it.
- Being itself a state value allows it to be fed with data incrementally or serve as context in the `State` monad.
- Parallel composition. Multiple reductions can be composed into one distributing the inputs between them.
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

{-|
Feeds all reductions, combining their results.

>>> :{
  extract $ feedList [1,2,3,4] $ 
  (,) <$> onTaken 2 list <*> onTaken 3 list
:}
([1,2],[1,2,3])
-}
instance Applicative (Reduction input) where
  pure = Terminated
  (<*>) = \ case
    Ongoing terminate1 consume1 -> \ case
      Ongoing terminate2 consume2 -> let
        terminate = terminate1 terminate2
        consume input = let
          nextReduction1 = consume1 input
          nextReduction2 = consume2 input
          in nextReduction1 <*> nextReduction2
        in Ongoing terminate consume
      Terminated output2 -> let
        terminate = terminate1 output2
        consume = fmap (\ output1 -> output1 output2) . consume1
        in Ongoing terminate consume
    Terminated output1 -> fmap output1

{-|
Feeds all reductions, terminating early if possible.
-}
instance Selective (Reduction input) where
  select = \ case
    Ongoing terminate1 consume1 -> \ case
      Ongoing terminate2 consume2 ->
        Ongoing
          (Prelude.either terminate2 id terminate1)
          (\ input -> select (consume1 input) (consume2 input))
      Terminated output2 ->
        Ongoing
          (Prelude.either output2 id terminate1)
          (fmap (Prelude.either output2 id) . consume1)
    Terminated output1 -> case output1 of
      Left output1 -> fmap ($ output1)
      Right output -> const (Terminated output)

{-|
Feeds all reductions, getting the result of the one that terminates first.
-}
instance Alt (Reduction input) where
  (<!>) = \ case
    Ongoing terminate1 consume1 -> \ case
      Ongoing terminate2 consume2 ->
        Ongoing terminate1 (\ i -> consume1 i <!> consume2 i)
      Terminated output2 -> Terminated output2
    Terminated output1 -> const (Terminated output1)

instance Comonad.Comonad (Reduction input) where
  extract = extract
  duplicate = \ case
    Ongoing terminate consume -> Ongoing (Ongoing terminate consume) (Comonad.duplicate . consume)
    Terminated output -> Terminated (Terminated output)

{-|
Allows to map over the input using `lmap`:

>>> list & lmap (+ 1) & feedList [1,2,3] & extract
[2,3,4]
-}
instance Profunctor Reduction where
  dimap proj1 proj2 = \ case
    Ongoing terminate consume -> Ongoing (proj2 terminate) (dimap proj1 proj2 . consume . proj1)
    Terminated output -> Terminated (proj2 output)
  lmap proj = \ case
    Ongoing terminate consume -> Ongoing terminate (lmap proj . consume . proj)
    Terminated output -> Terminated output
  rmap = fmap

instance Choice Reduction where
  right' = \ case
    Ongoing terminate consume ->
      Ongoing (Right terminate) $ \ case
        Left o -> Terminated (Left o)
        Right i -> right' (consume i)
    Terminated o -> Terminated (Right o)

instance Num b => Num (Reduction a b) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional b => Fractional (Reduction a b) where
  fromRational = pure . fromRational
  (/) = liftA2 (/)
  recip = fmap recip

instance Floating b => Floating (Reduction a b) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sqrt = fmap sqrt
  (**) = liftA2 (**)
  logBase = liftA2 logBase
  sin = fmap sin
  cos = fmap cos
  tan = fmap tan
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  tanh = fmap tanh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh
  log1p = fmap log1p
  expm1 = fmap expm1
  log1pexp = fmap log1pexp
  log1mexp = fmap log1mexp

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
Checks whether the input is empty.
-}
{-# INLINABLE null #-}
null :: Reduction a Bool
null = Ongoing True (const (Terminated False))

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
Gets the head.
-}
{-# INLINABLE head #-}
head :: Reduction a (Maybe a)
head = Ongoing Nothing (Terminated . Just)

{-|
Finds the first matching occurrence.
Same as @`onFiltered` predicate `head`@.
-}
{-# INLINABLE find #-}
find :: (a -> Bool) -> Reduction a (Maybe a)
find predicate = onFiltered predicate head

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

{-|
Reduction, collecting all visited elements into a strict list.
It's slower than `reverseStrictList`.
-}
{-# INLINABLE strictList #-}
strictList :: Reduction a (List a)
strictList = fmap StrictList.reverse reverseStrictList

{-|
Reduction, collecting all visited elements into a strict list in reverse order.
It's faster than `strictList`.
-}
{-# INLINABLE reverseStrictList #-}
reverseStrictList :: Reduction a (List a)
reverseStrictList = foldl (flip Cons) Nil

{-|
Reduction, collecting all visited elements into a generic vector.

>>> vector & feedList [1,2,3] & extract :: Data.Vector.Primitive.Vector Int
[1,2,3]
-}
{-# INLINABLE vector #-}
vector :: Vector vec a => Reduction a (vec a)
vector = liftA2 Vector.fromReverseStrictListN count reverseStrictList

{-|
Reduction, constructing a hashmap from pairs.
-}
{-# INLINABLE hashMap #-}
hashMap :: (Eq a, Hashable a) => Reduction (a, b) (HashMap a b)
hashMap = foldl (\ m (k, v) -> HashMap.insert k v m) HashMap.empty

{-|
Decode bytestring chunks using UTF-8,
producing Nothing in case of errors or unfinished input.

>>> decodeUtf8 & feedList ["\208", "\144\208", "\145\208", "\146"] & extract
Just "\1040\1041\1042"

>>> decodeUtf8 & feedList ["\208", "\144\208", "\145\208"] & extract
Nothing
-}
{-# INLINABLE decodeUtf8 #-}
decodeUtf8 :: Reduction ByteString (Maybe Text)
decodeUtf8 = onUtf8DecodedText (dimap TextBuilder.text TextBuilder.run concat)

-- *** Attoparsec
-------------------------

{-|
Convert an Attoparsec text parser into a reduction over text chunks.

>>> Data.Attoparsec.Text.decimal & parseText & feed "123" & feed "45" & extract
Right 12345
-}
{-# INLINABLE parseText #-}
parseText :: AttoText.Parser o -> Reduction Text (Either Text o)
parseText parser =
  Ongoing
    (extract (parserResult (AttoText.parse parser "")))
    (parserResult . AttoText.parse parser)

{-|
Convert an Attoparsec bytestring parser into a reduction over bytestring chunks.
-}
{-# INLINABLE parseByteString #-}
parseByteString :: AttoByteString.Parser o -> Reduction ByteString (Either Text o)
parseByteString parser =
  Ongoing
    (extract (parserResult (AttoByteString.parse parser "")))
    (parserResult . AttoByteString.parse parser)

{-# INLINABLE parserResult #-}
parserResult :: Monoid i => Atto.IResult i o -> Reduction i (Either Text o)
parserResult = let
  terminateCont cont = case cont mempty of
    Atto.Done _ o -> Right o
    Atto.Fail _ context details -> Left (Text.attoFailure context details)
    _ -> Left "Result: incomplete input"
  in \ case
    Atto.Partial cont -> Ongoing (terminateCont cont) (parserResult . cont)
    Atto.Done _ o -> Terminated (Right o)
    Atto.Fail _ context details -> Terminated (Left (Text.attoFailure context details))

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

>>> list & onTaken 2 & feedList [1,2,3,4] & extract
[1,2]
-}
{-# INLINABLE onTaken #-}
onTaken :: Int -> Reduction a b -> Reduction a b
onTaken amount = if amount > 0
  then mapOngoing (onTaken (pred amount))
  else forceTermination

{-|
Make reduction ignore the first elements.

>>> list & onDropped 2 & feedList [1,2,3,4] & extract
[3,4]
-}
{-# INLINABLE onDropped #-}
onDropped :: Int -> Reduction a b -> Reduction a b
onDropped amount = if amount > 0
  then \ reduction -> case reduction of
    Ongoing terminate consume -> Ongoing terminate (const (onDropped (amount - 1) reduction))
    _ -> reduction
  else id

{-|
>>> list & onTakenWhile (< 3) & feedList [1,2,3,4] & extract
[1,2]
-}
{-# INLINABLE onTakenWhile #-}
onTakenWhile :: (a -> Bool) -> Reduction a b -> Reduction a b
onTakenWhile predicate = let
  loop = \ case
    Ongoing terminate consume ->
      Ongoing terminate $ \ input -> if predicate input
        then loop (consume input)
        else Terminated terminate
    Terminated output -> Terminated output
  in loop

{-|
>>> list & onDroppedWhile (< 3) & feedList [1,2,3,4] & extract
[3,4]
-}
{-# INLINABLE onDroppedWhile #-}
onDroppedWhile :: (a -> Bool) -> Reduction a b -> Reduction a b
onDroppedWhile predicate = let
  loop = \ case
    Ongoing terminate consume ->
      Ongoing terminate $ \ input -> if predicate input
        then loop (Ongoing terminate consume)
        else consume input
    Terminated output -> Terminated output
  in loop

{-|
Generalization of `Data.List.partition`.

>>> onPartitions odd list list & feedList [1,2,3,4] & extract
([1,3],[2,4])
-}
{-# INLINABLE onPartitions #-}
onPartitions :: (a -> Bool) -> Reduction a b -> Reduction a c -> Reduction a (b, c)
onPartitions predicate reduction1 reduction2 =
  lmap (\ i -> if predicate i then Left i else Right i) $
  onEither reduction1 reduction2

{-|
Combines two reductions into one processing inputs for either of them.

>>> onEither list list & feedList [Left 1, Right 2, Left 3, Right 4] & extract
([1,3],[2,4])
-}
{-# INLINABLE onEither #-}
onEither :: Reduction a1 b1 -> Reduction a2 b2 -> Reduction (Either a1 a2) (b1, b2)
onEither = \ case
  Ongoing terminate1 consume1 -> \ case
    Ongoing terminate2 consume2 ->
      Ongoing
        (terminate1, terminate2)
        (\ case
          Left a1 -> onEither (consume1 a1) (Ongoing terminate2 consume2)
          Right a2 -> onEither (Ongoing terminate1 consume1) (consume2 a2)
        )
    Terminated output2 ->
      Ongoing
        (terminate1, output2)
        (\ case
          Left a1 -> onEither (consume1 a1) (Terminated output2)
          Right a2 -> onEither (Ongoing terminate1 consume1) (Terminated output2)
        )
  Terminated output1 -> \ case
    Ongoing terminate2 consume2 ->
      Ongoing
        (output1, terminate2)
        (\ case
          Right a2 -> onEither (Terminated output1) (consume2 a2)
          Left a1 -> onEither (Terminated output1) (Ongoing terminate2 consume2)
        )
    Terminated output2 -> Terminated (output1, output2)

{-|
Lift a reduction on each byte into a reduction on bytestring chunks.
-}
onByteStringBytes :: Reduction Word8 a -> Reduction ByteString a
onByteStringBytes = feedAndReduce feedByteString

{-|
Lift a reduction on each char into a reduction on text chunks.

>>> list & onTextChars & feedList ["ab", "c", "def"] & extract
"abcdef"
-}
onTextChars :: Reduction Char a -> Reduction Text a
onTextChars = feedAndReduce feedText

feedAndReduce :: (i2 -> Reduction i1 o -> Reduction i1 o) -> Reduction i1 o -> Reduction i2 o
feedAndReduce feed = let
  loop reduction = Ongoing (extract reduction) (\ chunk -> loop (feed chunk reduction))
  in loop

{-|
Lift a reduction of text chunks into a reduction of bytestrings,
outputting Nothing in case of encoding errors.
-}
{-# INLINABLE onUtf8DecodedText #-}
onUtf8DecodedText :: Reduction Text a -> Reduction ByteString (Maybe a)
onUtf8DecodedText = onTextDecoding (Text.Some mempty mempty Text.streamDecodeUtf8)

{-# INLINABLE onTextDecoding #-}
onTextDecoding :: Text.Decoding -> Reduction Text a -> Reduction ByteString (Maybe a)
onTextDecoding (Text.Some decodedChunk remainder cont) = \ case
  Ongoing terminate consume ->
    Ongoing
      (if ByteString.null remainder
        then if Text.null decodedChunk
          then Just terminate
          else Just (extract (consume decodedChunk))
        else Nothing
      )
      (\ remainder -> unsafeDupablePerformIO (catch
        (fmap
          (\ decoding -> onTextDecoding decoding (consume decodedChunk))
          (evaluate (cont remainder)))
        (\ case
          Text.DecodeError _ _ -> return (Terminated Nothing)
          _ -> error "Unexpected EncodeError"
        )))
  Terminated output -> Terminated (Just output)

{-# INLINABLE onTextDecodingStep #-}
onTextDecodingStep :: (ByteString -> Text.Decoding) -> Reduction Text a -> Reduction ByteString (Maybe a)
onTextDecodingStep step = \ case
  Ongoing terminate consume ->
    Ongoing
      (Just terminate)
      (\ bytes -> unsafeDupablePerformIO (catch
        (do
          Text.Some text _ nextStep <- evaluate (step bytes)
          return (onTextDecodingStep nextStep (consume text))
        )
        (\ case
          Text.DecodeError _ _ -> return (Terminated Nothing)
          _ -> error "Unexpected EncodeError"
        )))
  Terminated output -> Terminated (Just output)

{-|
Focus a reduction on unique inputs.

>>> list & onUnique & feedList [1,2,1,3] & extract
[1,2,3]
-}
{-# INLINABLE onUnique #-}
onUnique :: (Eq a, Hashable a) => Reduction a b -> Reduction a b
onUnique = let
  alteration = \ case
    Just _ -> (True, Just ())
    Nothing -> (False, Just ())
  loop !map = \ case
    Ongoing terminate consume ->
      Ongoing terminate $ \ i -> let
        (exists, newMap) = HashMap.alterF alteration i map
        in if exists
          then loop newMap (Ongoing terminate consume)
          else loop newMap (consume i)
    Terminated output -> Terminated output
  in loop HashMap.empty

{-|
Focus a reduction on filtered inputs.

>>> list & onFiltered odd & feedList [1,2,3,4,5] & extract
[1,3,5]
-}
{-# INLINABLE onFiltered #-}
onFiltered :: (a -> Bool) -> Reduction a b -> Reduction a b
onFiltered predicate = let
  loop = \ case
    Ongoing terminate consume ->
      Ongoing terminate $ \ i -> if predicate i
        then loop (consume i)
        else loop (Ongoing terminate consume)
    Terminated output -> Terminated output
  in loop

-- *** Attoparsec integration
-------------------------

{-|
Parse a stream of values, reducing it to a final result.

>>> :{
  let
    parser = Data.Attoparsec.Text.decimal <* Data.Attoparsec.Text.char ','
    in list & onParsedText parser & feedList ["12,", "3", ",4,"] & extract
:}
Right [12,3,4]

>>> :{
  let
    parser = Data.Attoparsec.Text.decimal <* Data.Attoparsec.Text.char ','
    in list & onParsedText parser & extract
:}
Right []
-}
{-# INLINABLE onParsedText #-}
onParsedText :: AttoText.Parser a -> Reduction a b -> Reduction Text (Either Text b)
onParsedText parser = let
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
    Atto.Fail _ context details -> const (Terminated (Left (Text.attoFailure context details)))
  in handleResult (AttoText.parse parser "")

{-|
Parse a stream of values, reducing it to a final result.
-}
{-# INLINABLE onParsedByteString #-}
onParsedByteString :: AttoByteString.Parser a -> Reduction a b -> Reduction ByteString (Either Text b)
onParsedByteString parser = let
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
    Atto.Fail _ context details -> const (Terminated (Left (Text.attoFailure context details)))
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
Update reduction by feeding a strict list of inputs to it.
-}
{-# INLINABLE feedStrictList #-}
feedStrictList :: List a -> Reduction a output -> Reduction a output
feedStrictList = \ case
  Cons input remainingList -> \ case
    Ongoing _ consume -> consume input & feedStrictList remainingList
    terminated -> terminated
  _ -> id

{-|
Update reduction by feeding a vector of inputs to it.
-}
{-# INLINABLE feedVector #-}
feedVector :: Vector vec input => vec input -> Reduction input output -> Reduction input output
feedVector = feedIndexable Vec.length Vec.unsafeIndex

{-|
Update reduction by feeding each byte of a bytestring to it.
-}
{-# INLINABLE feedByteString #-}
feedByteString :: ByteString -> Reduction Word8 output -> Reduction Word8 output
feedByteString = feedIndexable ByteString.length ByteString.unsafeIndex

{-|
Update reduction by feeding each character of a text to it.

>>> list & feedText "АБВГ" & extract
"\1040\1041\1042\1043"
-}
{-# INLINABLE feedText #-}
feedText :: Text -> Reduction Char output -> Reduction Char output
feedText text = let
  length = Text.lengthWord16 text
  iterate index = \ case
    Ongoing terminate consume -> if index < length
      then let
        Text.Iter char delta = Text.iter text index
        in iterate (index + delta) (consume char)
      else Ongoing terminate consume
    terminated -> terminated
  in iterate 0

{-|
Update reduction by feeding a vector of inputs to it.
-}
{-# INLINE feedIndexable #-}
feedIndexable :: (indexable -> Int) -> (indexable -> Int -> input) -> indexable -> Reduction input output -> Reduction input output
feedIndexable getLength getElement indexable = let
  length = getLength indexable
  iterate index = \ case
    Ongoing terminate consume -> if index < length
      then iterate (succ index) (consume (getElement indexable index))
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
