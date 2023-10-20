{-# LANGUAGE FlexibleInstances #-}
-- Author: Colin McDonald
-- Description:
-- > Implements Cantor-style 1-to-1 correspondence between many classes
-- > of types and the natural numbers (given by their index in enumerate).
-- Known bugs:
-- 1. fromIndex on numbers greater than E+308 (max double size?) seem hang the program
module Cantor where
import Debug.Trace (trace)

-- If maxBounds is Bounded, then treat any
-- bounded type of greater size as unbounded;
-- if Unbounded, don't change any behavior
-- This makes types like Int and Char behave
-- more reasonably
maxBounds :: Bounds ()
maxBounds = Bounded (2 ^ 20) -- Unbounded

castBounds :: Bounds a -> Bounds b
castBounds Unbounded = Unbounded
castBounds (Bounded m) = Bounded m

caseBounded :: Bounds a -> (Integer -> b) -> b -> b
caseBounded Unbounded b u = u
caseBounded (Bounded m) b u = b m

type Stream a = [a]

data Bounds a = Bounded Integer | Unbounded
bounded x = case maxBounds of
  Bounded y | x >= y -> Unbounded
  _ -> Bounded x

joinBounds :: (Integer -> Integer -> Integer) -> Bounds a -> Bounds b -> Bounds c
joinBounds f Unbounded _ = Unbounded
joinBounds f _ Unbounded = Unbounded
joinBounds f (Bounded a) (Bounded b) = Bounded (f a b)

drop' :: Integer -> [a] -> [a]
drop' 0 as = as
drop' n (_ : as) = drop' (pred n) as

take' :: Integer -> [a] -> [a]
take' 0 as = []
take' n (a : as) = a : take' (pred n) as

nth :: Integer -> [a] -> a
nth 0 (a : as) = a
nth n (a : as) = nth (pred n) as

length' :: [a] -> Integer
length' = h 0 where
  h acc [] = acc
  h acc (_ : as) = h (succ acc) as

sublist :: Integer -> Integer -> [a] -> [a]
sublist s e as = take' (e - s) (drop' s as)

class Countable a where
  enumerate :: Stream a
  fromIndex :: Integer -> a
  toIndex :: a -> Integer
  size :: Bounds a

  {-# MINIMAL (enumerate | fromIndex), toIndex, size #-}

  enumerate = h size where
    h :: Countable a => Bounds a -> Stream a
    h Unbounded = [fromIndex i | i <- [0..]]
    h (Bounded m) = [fromIndex i | i <- [0..m-1]]
  fromIndex i = (nth i :: Stream a -> a) enumerate

  enumerateFromTo :: a -> a -> Stream a
  enumerateFrom :: a -> Stream a
  enumerateTo :: a -> Stream a

  enumerateFrom a = drop' (toIndex a - 1) enumerate
  enumerateTo a = take' (toIndex a) enumerate
  enumerateFromTo a b = sublist (toIndex a) (toIndex b + 1) enumerate

diagonalize :: (Stream a, Stream b) -> Stream (a, b)
diagonalize (as, bs) = h as bs [] [] where
  --h :: Stream a -> Stream b -> [a] -> [b] -> Stream (a, b)
  h [] [] pas pbs = []
  h (a : as) (b : bs) pas pbs =
    zip (a : pas) (pbs ++ [b]) ++ h as bs (a : pas) (pbs ++ [b])
  h (a : as) [] pas pbs =
    -- zip auto-truncates to lesser length
    zip (a : pas) pbs ++ h as [] (a : pas) pbs
  h [] (b : bs) pas pbs =
    zip pas (tail pbs ++ [b]) ++ h [] bs pas (tail pbs ++ [b])

--assumes a and b are unbounded types
undiagonalize :: Integer -> Integer -> Integer
undiagonalize i j = j + (((i + j) * (i + j + 1)) `div` 2)

--Powerset of a potentially infinite-length list
--N.B. this only produces finite-size members
--(e.g. Evens \notin powerset(Nats))
powerset :: Stream a -> Stream [a]
powerset xs =
  [] : foldr
    (\ x next acc ->
       let acc' = map (\ xs -> xs ++ [x]) acc in
         acc' ++ (next (acc ++ acc')))
    (\ _ -> []) xs [[]]

-- No repetitions (set)
permutations :: Stream a -> Stream [a]
permutations xs =
  [] : foldr
    (\ x next acc ->
       let acc' = concatMap (insertions x) acc in
         acc' ++ (next (acc ++ acc')))
    (\ _ -> []) xs [[]]

--Returns a list (of lists) of all the places we can insert an element into a list
insertions :: a -> [a] -> [[a]]
insertions a as = insertionsh a as [] where
  insertionsh :: a -> [a] -> [a] -> [[a]]
  insertionsh a [] bs' = [reverse (a : bs')]
  insertionsh a (b : bs) bs' =
    (reverse bs' ++ a : b : bs) : insertionsh a bs (b : bs')

-- Computes the nth tuple in the diagonal table:
--  /——————————————————\
--  | /—————————————\   \
--  | | /————————\   \   \
--  | | |row *0* 1   2   3
--  | | |    /   /   /   /
--  | | | 0,0 1,0 2,0 3,0 ...
--  | | \—/  /   /   /
--  | \   0,1 1,1 2,1
--  |  \——/  /   /
--  \     0,2 1,2
--   \————/  /
--        0,3
-- <——————/
pascalsPosition :: Integer -> (Integer, Integer)
pascalsPosition n =
  let row = floor (-0.5 + sqrt (0.25 + 2 * fromInteger n))
      col = n - ((row * (row + 1)) `div` 2) in
    (row - col, col)

-- Invariant: for each x in sumlen n, sum x + length x = n
-- e.g. sumlen 3 = [[2], [0, 1], [1, 0], [0, 0, 0]]
-- Returned list has 2^(n-1) elements
sumlen :: Integer -> [[Integer]]
sumlen n = concat [sumlenh (n - k) k | k <- [0..n]]
  where
    -- Fixes each sublist to have exactly k members
    sumlenh :: Integer -> Integer -> [[Integer]]
    sumlenh 0 k = [[0 | _ <- [0..k-1]]]
    sumlenh n 0 = []
    sumlenh n 1 = [[n]]
    sumlenh n k = concat [map ((:) i) (sumlenh (n - i) (k - 1)) | i <- [0..n]]

-- Quickly computes length (sumlenh n k) = 1/n! * Π_{i=1}^n (k + i - 1)
numlen :: Integer -> Integer -> Integer
numlen n k = product [k + i - 1 | i <- [1..n]] `div` product [1..n]
--numlen 0 k = 1  
--numlen n k = (numlen (n - 1) k * (n + k - 1)) `div` n

-- Returns the index that list "is" is in sumlenh (sum is) (length is)
sumlenIndex :: [Integer] -> Integer
sumlenIndex is = h (sum is) (length' is) is
  where
    h n k [] = 0
    h n k (i : is) =
      sum [numlen (n - j) (k - 1) | j <- [0..i-1]] + h (n - i) (k - 1) is

nthsumlen :: Integer -> Integer -> Integer -> [Integer]
nthsumlen s 0 n = []
--nthsumlen s 1 n = [s]
--nthsumlen s 1 0 = [s]
--nthsumlen s 1 n = error ("nthsumlen " ++ show s ++ " 1 " ++ show n)
nthsumlen s l n = h 0 n where
  h :: Integer -> Integer -> [Integer]
  h m n =
    --trace ("(h " ++ show m ++ " " ++ show n ++ ")") $
    let nl = numlen (s - m) (l - 1) in
      if n < nl then m : nthsumlen (s - m) (l - 1) n else h (1 + m) (n - nl)

starx :: Bounds a -> Integer -> [Integer]
starx b 0 = []
starx Unbounded i =
  let sl = 1 + floor (log (fromInteger i) / log 2)
      i' = i - 2 ^ (sl - 1)
      row = sl - 1 in
    if i' == 2 ^ (sl - 1) - 1 then
      [0 | _ <- [0..sl - 1]]
    else if i' == 0 then
      [sl - 1]
    else
      let h len j = let j' = j - (row `choose` len) in if j' <= 0 then (len + 1, j - 1) else h (len + 1) j'
          (len, i'') = h 1 i'
          s = sl - len
      in
        nthsumlen s len i''
starx (Bounded m) i =
  let k = floor (log (fromInteger (i * (m - 1) + 1)) / log (fromInteger m)) -- number of elements
      x' = (m ^ (k + 0)) `div` (m - 1) -- number of lists of length at most k-1
      x = i - x' + 1 -- index within lists of length k
  in
    [(x `div` (m ^ (k - j - 1))) `mod` m | j <- [0..k-1]]

-- Yields all possible lists with elements of a stream.
-- Each Integer is an index into that stream.
-- Follows Pascal's Triangle for unbounded types,
-- where at row n and column k there are n `choose` k elements,
-- drawn from sumlenh n k.
-- For bounded types, simply enumerates all the 0-length lists,
-- then 1-length lists, 2-length lists, etc.
star :: Bounds a -> [[Integer]]
star Unbounded = concat [sumlen n | n <- [0..]]
star b@(Bounded m) = h 0 where
  alllen :: Integer -> [[Integer]]
  alllen 0 = [[]]
  alllen n = concat [map ((:) i) (alllen (pred n)) | i <- [0..m-1]]
  h n = alllen n ++ h (succ n)

choose :: Integer -> Integer -> Integer
n `choose` k = product [n, n-1 .. n - k + 1] `div` product [k, k-1 .. 1]

unstar :: Countable a => Bounds a -> [a] -> Integer
unstar b [] = 0
unstar Unbounded as =
  let is = [toIndex a | a <- as]
      s = sum is
      l = length' as
      row = s + l - 1 -- row of Pascal's Triangle
      --col = l
      prev_in_rows = 2 ^ row - 1 -- how many before this row
      prev_in_cols = sum [row `choose` (col - 1) | col <- [1..l - 1]] -- how many before this column within this row
  in
    -- add one because unstar [] = 0
    1 + prev_in_rows + prev_in_cols + sumlenIndex is
unstar (Bounded i) as =
  let l = length as
      prev = if i == 2 then (2 ^ l - 1) else ((i ^ l) `div` (i - 1))
      (this, _) = foldr (\a (c, p) -> (toIndex a * p + c, p * i)) (0, 1) as
  in
    prev + this

alternate :: [a] -> [a] -> [a]
alternate (a : as) bs = a : alternate bs as
alternate [] bs = bs

deriveBoundedEnumerate :: (Bounded a, Enum a) => [a]
deriveBoundedEnumerate = [minBound..maxBound]

deriveBoundedToIndex :: Enum a => a -> Integer
deriveBoundedToIndex = toInteger . fromEnum

deriveBoundedFromIndex :: Enum a => Integer -> a
deriveBoundedFromIndex = toEnum . fromInteger

deriveBoundedSize :: (Bounded a, Enum a) => Bounds a
deriveBoundedSize = indexAsBound maxBound
  where
    indexAsBound :: Enum a => a -> Bounds a
    indexAsBound a = bounded (succ (toInteger (fromEnum a)))

zplus :: (Enum n, Num n) => [n]
zplus = [1, 2..] -- strictly positive integers
zminus :: (Enum n, Num n) => [n]
zminus = [-1, -2..] -- strictly negative integers

-- 1-to-1 mapping from a countable type to another countable type
map1to1 :: (Countable a, Countable b) => a -> b
map1to1 = fromIndex . toIndex

instance Countable Bool where
  enumerate = deriveBoundedEnumerate
  toIndex = deriveBoundedToIndex
  fromIndex = deriveBoundedFromIndex
  size = deriveBoundedSize

instance Countable () where
  enumerate = deriveBoundedEnumerate
  toIndex = deriveBoundedToIndex
  fromIndex = deriveBoundedFromIndex
  size = deriveBoundedSize

instance Countable Char where
  enumerate = deriveBoundedEnumerate
  toIndex = deriveBoundedToIndex
  fromIndex = deriveBoundedFromIndex
  size = deriveBoundedSize

instance Countable Int where
  enumerate = 0 : alternate zplus zminus
  toIndex i
    | i <  0 = -2 * toInteger i
    | i == 0 = 0
    | i >  0 = 2 * toInteger i - 1
  fromIndex j
    | j == 0 = 0
    | odd j = fromInteger ((j + 1) `div` 2)
    | even j = fromInteger (j `div` (-2))
  size = bounded (1 + toInteger (maxBound :: Int) - toInteger (minBound :: Int))

instance Countable Integer where
  enumerate = 0 : alternate zplus zminus
  toIndex i
    | i <  0 = -2 * i
    | i == 0 = 0
    | i >  0 = 2 * i - 1
  fromIndex j
    | j == 0 = 0
    | odd  j = (j + 1) `div` 2
    | even j = j `div` (-2)
  size = Unbounded

instance Countable a => Countable [a] where
  --enumerate = map (maybe [] (uncurry (:))) enumerate
  --toIndex = (\as -> toIndex (case as of a : as -> Just (a, as); [] -> Nothing)) :: Countable a => [a] -> Integer
  --fromIndex i = maybe [] (uncurry (:)) (fromIndex i)
  enumerate = ((map (map fromIndex) . star) :: Countable a => Bounds a -> Stream [a]) size
  toIndex = unstar size
  fromIndex i = ((\b -> map fromIndex (starx b i)) :: Countable a => Bounds a -> [a]) size
  size = Unbounded
  
instance (Countable a, Countable b) => Countable (Either a b) where
  enumerate = h size size where
    h :: (Countable a, Countable b) => Bounds a -> Bounds b -> Stream (Either a b)
    h (Bounded _) (Bounded _) =
      map Left enumerate ++ map Right enumerate
    h (Bounded _) Unbounded =
      map Left enumerate ++ map Right enumerate
    h Unbounded (Bounded _) =
      map Right enumerate ++ map Left enumerate
    h Unbounded Unbounded =
      alternate (map Left enumerate) (map Right enumerate)
  
  fromIndex j = h size size where
    h :: (Countable a, Countable b) => Bounds a -> Bounds b -> Either a b
    h (Bounded a) (Bounded b)
      | j < a = Left (fromIndex j)
      | otherwise = Right (fromIndex (j - a))
    h (Bounded a) Unbounded
      | j < a = Left (fromIndex j)
      | otherwise = Right (fromIndex (j - a))
    h Unbounded (Bounded b)
      | j < b = Right (fromIndex j)
      | otherwise = Left (fromIndex (j - b))
    h Unbounded Unbounded
      | even j = Left (fromIndex (j `div` 2))
      | otherwise = Right (fromIndex (j `div` 2))

  toIndex = either (\a -> 2 * toIndex a) (\b -> 1 + 2 * toIndex b)
  size = (joinBounds (+) :: Bounds a -> Bounds b -> Bounds (Either a b)) size size

instance Countable a => Countable (Maybe a) where
  enumerate = Nothing : fmap Just enumerate
  toIndex = maybe 0 (\a -> 1 + toIndex a)
  fromIndex j
    | j == 0 = Nothing
    | j >= 1 = Just (fromIndex (j - 1))
  size = (castBounds :: Bounds a -> Bounds (Maybe a)) size

instance (Countable a, Countable b) => Countable (a, b) where
  enumerate = h size size where
    h :: (Countable a, Countable b) => Bounds a -> Bounds b -> Stream (a, b)
    h (Bounded _) (Bounded _) = [(a, b) | a <- enumerate, b <- enumerate]
    h (Bounded _) Unbounded = [(a, b) | b <- enumerate, a <- enumerate]
    h Unbounded (Bounded _) = [(a, b) | a <- enumerate, b <- enumerate]
    h Unbounded Unbounded = diagonalize (enumerate, enumerate)
  toIndex (a, b) = h size size a b (toIndex a) (toIndex b) where
    h :: Bounds a -> Bounds b -> a -> b -> Integer -> Integer -> Integer
    h (Bounded as) (Bounded bs) a b i j = i * bs + j
    h (Bounded as) Unbounded a b i j = j * as + i
    h Unbounded (Bounded bs) a b i j = i * bs + j
    h Unbounded Unbounded a b i j = undiagonalize i j
  fromIndex j = h size size where
    h :: (Countable a, Countable b) => Bounds a -> Bounds b -> (a, b)
    h (Bounded as) (Bounded bs) = (fromIndex (j `div` bs), fromIndex (j `mod` bs))
    h (Bounded as) Unbounded = (fromIndex (j `mod` as), fromIndex (j `div` as))
    h Unbounded (Bounded bs) = (fromIndex (j `div` bs), fromIndex (j `mod` bs))
    h Unbounded Unbounded =
      let (a, b) = pascalsPosition j in (fromIndex a, fromIndex b)
  size = (joinBounds (*) :: Bounds a -> Bounds b -> Bounds (a, b)) size size

instance (Countable a, Countable b, Countable c) => Countable (a, b, c) where
  enumerate = fmap (\ ((a, b), c) -> (a, b, c)) enumerate
  toIndex (a, b, c) = toIndex ((a, b), c)
  fromIndex j = let ((a, b), c) = fromIndex j in (a, b, c)
  size = (joinBounds (*) :: Bounds (a, b) -> Bounds c -> Bounds (a, b, c)) size size

instance (Countable a, Countable b, Countable c, Countable d) => Countable (a, b, c, d) where
  enumerate = fmap (\ ((a, b, c), d) -> (a, b, c, d)) enumerate
  toIndex (a, b, c, d) = toIndex ((a, b, c), d)
  fromIndex j = let ((a, b, c), d) = fromIndex j in (a, b, c, d)
  size = (joinBounds (*) :: Bounds (a, b, c) -> Bounds d -> Bounds (a, b, c, d)) size size

instance (Countable a, Countable b, Countable c, Countable d, Countable e) => Countable (a, b, c, d, e) where
  enumerate = fmap (\ ((a, b, c, d), e) -> (a, b, c, d, e)) enumerate
  toIndex (a, b, c, d, e) = toIndex ((a, b, c, d), e)
  fromIndex j = let ((a, b, c, d), e) = fromIndex j in (a, b, c, d, e)
  size = (joinBounds (*) :: Bounds (a, b, c, d) -> Bounds e -> Bounds (a, b, c, d, e)) size size

instance (Countable a, Countable b, Countable c, Countable d, Countable e, Countable f) => Countable (a, b, c, d, e, f) where
  enumerate = fmap (\ ((a, b, c, d, e), f) -> (a, b, c, d, e, f)) enumerate
  toIndex (a, b, c, d, e, f) = toIndex ((a, b, c, d, e), f)
  fromIndex j = let ((a, b, c, d, e), f) = fromIndex j in (a, b, c, d, e, f)
  size = (joinBounds (*) :: Bounds (a, b, c, d, e) -> Bounds f -> Bounds (a, b, c, d, e, f)) size size

instance (Countable a, Countable b, Countable c, Countable d, Countable e, Countable f, Countable g) => Countable (a, b, c, d, e, f, g) where
  enumerate = fmap (\ ((a, b, c, d, e, f), g) -> (a, b, c, d, e, f, g)) enumerate
  toIndex (a, b, c, d, e, f, g) = toIndex ((a, b, c, d, e, f), g)
  fromIndex j = let ((a, b, c, d, e, f), g) = fromIndex j in (a, b, c, d, e, f, g)
  size = (joinBounds (*) :: Bounds (a, b, c, d, e, f) -> Bounds g -> Bounds (a, b, c, d, e, f, g)) size size

instance (Countable a, Countable b, Countable c, Countable d, Countable e, Countable f, Countable g, Countable h) => Countable (a, b, c, d, e, f, g, h) where
  enumerate = fmap (\ ((a, b, c, d, e, f, g), h) -> (a, b, c, d, e, f, g, h)) enumerate
  toIndex (a, b, c, d, e, f, g, h) = toIndex ((a, b, c, d, e, f, g), h)
  fromIndex j = let ((a, b, c, d, e, f, g), h) = fromIndex j in (a, b, c, d, e, f, g, h)
  size = (joinBounds (*) :: Bounds (a, b, c, d, e, f, g) -> Bounds h -> Bounds (a, b, c, d, e, f, g, h)) size size

-- ...
