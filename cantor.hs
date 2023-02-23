{-# LANGUAGE FlexibleInstances #-}
-- Author: Colin McDonald
-- Description:
-- > Implements Cantor-style 1-to-1 correspondence between many classes
-- > of types and the natural numbers (given by their index in enumerate).
module Cantor where

-- TODO: perhaps implement a faster fromIndex function,
-- instead of doing nth on enumerate?

-- If maxBounds is Just, then treat any
-- bounded type of greater size as unbounded;
-- if Nothing, don't change any behavior
-- This makes types like Int and Char produce
-- more reasonable numbers
maxBounds :: Maybe Integer
maxBounds = Just (2 ^ 20)
--maxBounds = Nothing

type Stream a = [a]

newtype Bounds a = Bounds (Maybe Integer)
unbounded = Bounds Nothing
bounded x = case maxBounds of
  Just y | x >= y -> Bounds Nothing
  _ -> Bounds (Just x)

joinBounds :: (Integer -> Integer -> Integer) -> Bounds a -> Bounds b -> Bounds c
joinBounds f (Bounds a) (Bounds b) = Bounds (pure f <*> a <*> b)

isBounded :: Bounds a -> Bool
isBounded (Bounds Nothing) = False
isBounded (Bounds (Just _)) = True

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
  toIndex :: a -> Integer
  size :: Bounds a

  {-# MINIMAL enumerate, toIndex, size #-}

  enumerateFromTo :: a -> a -> Stream a
  enumerateFrom :: a -> Stream a
  enumerateTo :: a -> Stream a

  enumerateFrom a = drop' (toIndex a - 1) enumerate
  enumerateTo a = take' (toIndex a) enumerate
  enumerateFromTo a b = sublist (toIndex a) (toIndex b + 1) enumerate

-- getIndex :: (Eq a, Countable a) => a -> Integer
-- getIndex a = h 0 enumerate where
--   h i (a' : as)
--    | a == a' = i
--    | otherwise = h (succ i) as
--   h i [] = error "Error: element not in enumeration! You probably have a bad Countable instance definition"

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

-- Invariant: for each x in sumlen n, sum x + length x = n
-- e.g. sumlen 3 = [[2], [0, 1], [1, 0], [0, 0, 0]]
-- Returned list has 2^(n-1) elements
sumlen :: Integer -> [[Integer]]
sumlen n = concat [sumlenh (n - k) k | k <- [0..n]]
--  where
-- Fixes each sublist to have exactly k members
sumlenh :: Integer -> Integer -> [[Integer]]
sumlenh 0 k = [[0 | _ <- [0..k-1]]]
sumlenh n 0 = []
sumlenh n 1 = [[n]]
sumlenh n k = concat [map ((:) i) (sumlenh (n - i) (k - 1)) | i <- [0..n]]

indexOf a [] = error "element not in list"
indexOf a (a' : as)
  | a == a' = 0
  | otherwise = succ (indexOf a as)

-- Yields all possible lists with elements of a stream
-- Follows Pascal's Triangle for unbounded types,
-- where at row n and column k there are n `choose` k elements,
-- drawn from sumlenh n k.
-- For bounded types, simply enumerates all the 0-length lists,
-- then 1-length lists, 2-length lists, etc.
star :: Bounds a -> Stream a -> Stream [a]
star (Bounds Nothing) xs = map (map (\i -> nth i xs)) (concat [sumlen n | n <- [0..]])
star b@(Bounds (Just _)) xs = h 0 where
  alllen :: Integer -> Stream a -> [[a]]
  alllen 0 xs = [[]]
  alllen n xs = concat [map ((:) x) (alllen (pred n) xs) | x <- xs]
  h n = alllen n xs ++ h (succ n)

choose :: Integer -> Integer -> Integer
n `choose` k = product [n, n-1 .. n - k + 1] `div` product [k, k-1 .. 1]

-- Returns the index that this is in sumlenh n k
sumlenIndex :: Integer -> Integer -> [Integer] -> Integer
sumlenIndex n k is = indexOf is (sumlenh n k) -- TODO: make more efficient (e.g. math-based)

unstar :: Countable a => Bounds a -> [a] -> Integer
unstar b [] = 0
unstar (Bounds Nothing) as =
  let is = [toIndex a | a <- as]
      s = sum is
      l = length' as
      row = s + l -- row of Pascal's Triangle
      col = l
      prev_in_rows = 2 ^ (row - 1) - 1 -- how many before this row
      prev_in_cols = sum [(row - 1) `choose` (col - 1) | col <- [1..l - 1]] -- how many before this column within this row
  in
    -- add one because unstar [] = 0
    1 + prev_in_rows + prev_in_cols + sumlenIndex s l is
unstar (Bounds (Just i)) as =
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

deriveBoundedIndex :: Enum a => a -> Integer
deriveBoundedIndex = toInteger . fromEnum

deriveBoundedSize :: (Bounded a, Enum a) => Bounds a
deriveBoundedSize = indexAsBound maxBound
  where
    indexAsBound :: Enum a => a -> Bounds a
    indexAsBound a = bounded (succ (toInteger (fromEnum a)))

zplus :: (Enum n, Num n) => [n]
zplus = [1, 2..] -- strictly positive integers
zminus :: (Enum n, Num n) => [n]
zminus = [-1, -2..] -- strictly negative integers

instance Countable Bool where
  enumerate = deriveBoundedEnumerate
  toIndex = deriveBoundedIndex
  size = deriveBoundedSize

instance Countable () where
  enumerate = deriveBoundedEnumerate
  toIndex = deriveBoundedIndex
  size = deriveBoundedSize

instance Countable Char where
  enumerate = deriveBoundedEnumerate
  toIndex = deriveBoundedIndex
  size = deriveBoundedSize

instance Countable Word where
  enumerate = deriveBoundedEnumerate
  toIndex = deriveBoundedIndex
  size = deriveBoundedSize

instance Countable Ordering where
  enumerate = deriveBoundedEnumerate
  toIndex = deriveBoundedIndex
  size = deriveBoundedSize

instance Countable Int where
  enumerate = 0 : alternate zplus zminus
  toIndex i
    | i <  0 = -2 * toInteger i
    | i == 0 = 0
    | i >  0 = 2 * toInteger i - 1
  size = bounded (1 + toInteger (maxBound :: Int) - toInteger (minBound :: Int))

instance Countable Integer where
  enumerate = 0 : alternate zplus zminus
  toIndex i
    | i <  0 = -2 * i
    | i == 0 = 0
    | i >  0 = 2 * i - 1
  size = unbounded

instance Countable a => Countable [a] where
  enumerate = star size enumerate
  toIndex = unstar size
  size = unbounded

instance (Countable a, Countable b) => Countable (Either a b) where
  enumerate = ((\a b -> case (isBounded a, isBounded b) of
                   (True, True)   -> map Left  enumerate ++ map Right enumerate
                   (True, False)  -> map Left  enumerate ++ map Right enumerate
                   (False, True)  -> map Right enumerate ++ map Left  enumerate
                   (False, False) -> alternate (map Left enumerate) (map Right enumerate))
               :: (Countable a, Countable b) => Bounds a -> Bounds b -> Stream (Either a b))
              size size
  toIndex = either (\a -> 2 * toIndex a) (\b -> 1 + 2 * toIndex b)
  size = (joinBounds (+) :: Bounds a -> Bounds b -> Bounds (Either a b)) size size

instance Countable a => Countable (Maybe a) where
  enumerate = Nothing : fmap Just enumerate
  toIndex = maybe 0 (\a -> 1 + toIndex a)
  size = ((\(Bounds m) -> Bounds m) :: Bounds a -> Bounds (Maybe a)) size

instance (Countable a, Countable b) => Countable (a, b) where
  enumerate = ((\a b -> case (isBounded a, isBounded b) of
                   (True, True)   -> [(a, b) | a <- enumerate, b <- enumerate]
                   (True, False)  -> [(a, b) | b <- enumerate, a <- enumerate]
                   (False, True)  -> [(a, b) | a <- enumerate, b <- enumerate]
                   (False, False) -> diagonalize (enumerate, enumerate))
               :: (Countable a, Countable b) => Bounds a -> Bounds b -> Stream (a, b))
              size size
  toIndex ab =
      ((\(Bounds ar) (Bounds br) (a, b) i j ->
          case (ar, br) of
            (Just as, Just bs) -> i * bs + j
            (Just as, Nothing) -> j * as + i
            (Nothing, Just bs) -> i * bs + j
            (Nothing, Nothing) -> undiagonalize i j)
        :: (Countable a, Countable b) => Bounds a -> Bounds b -> (a, b) -> Integer -> Integer -> Integer)
      size size ab (toIndex (fst ab)) (toIndex (snd ab))
  size = (joinBounds (*) :: Bounds a -> Bounds b -> Bounds (a, b)) size size

instance (Countable a, Countable b, Countable c) => Countable (a, b, c) where
  enumerate = fmap (\ ((a, b), c) -> (a, b, c)) enumerate
  toIndex (a, b, c) = toIndex ((a, b), c)
  size = (joinBounds (*) :: Bounds (a, b) -> Bounds c -> Bounds (a, b, c)) size size

instance (Countable a, Countable b, Countable c, Countable d) => Countable (a, b, c, d) where
  enumerate = fmap (\ ((a, b, c), d) -> (a, b, c, d)) enumerate
  toIndex (a, b, c, d) = toIndex ((a, b, c), d)
  size = (joinBounds (*) :: Bounds (a, b, c) -> Bounds d -> Bounds (a, b, c, d)) size size

instance (Countable a, Countable b, Countable c, Countable d, Countable e) => Countable (a, b, c, d, e) where
  enumerate = fmap (\ ((a, b, c, d), e) -> (a, b, c, d, e)) enumerate
  toIndex (a, b, c, d, e) = toIndex ((a, b, c, d), e)
  size = (joinBounds (*) :: Bounds (a, b, c, d) -> Bounds e -> Bounds (a, b, c, d, e)) size size

instance (Countable a, Countable b, Countable c, Countable d, Countable e, Countable f) => Countable (a, b, c, d, e, f) where
  enumerate = fmap (\ ((a, b, c, d, e), f) -> (a, b, c, d, e, f)) enumerate
  toIndex (a, b, c, d, e, f) = toIndex ((a, b, c, d, e), f)
  size = (joinBounds (*) :: Bounds (a, b, c, d, e) -> Bounds f -> Bounds (a, b, c, d, e, f)) size size

instance (Countable a, Countable b, Countable c, Countable d, Countable e, Countable f, Countable g) => Countable (a, b, c, d, e, f, g) where
  enumerate = fmap (\ ((a, b, c, d, e, f), g) -> (a, b, c, d, e, f, g)) enumerate
  toIndex (a, b, c, d, e, f, g) = toIndex ((a, b, c, d, e, f), g)
  size = (joinBounds (*) :: Bounds (a, b, c, d, e, f) -> Bounds g -> Bounds (a, b, c, d, e, f, g)) size size

instance (Countable a, Countable b, Countable c, Countable d, Countable e, Countable f, Countable g, Countable h) => Countable (a, b, c, d, e, f, g, h) where
  enumerate = fmap (\ ((a, b, c, d, e, f, g), h) -> (a, b, c, d, e, f, g, h)) enumerate
  toIndex (a, b, c, d, e, f, g, h) = toIndex ((a, b, c, d, e, f, g), h)
  size = (joinBounds (*) :: Bounds (a, b, c, d, e, f, g) -> Bounds h -> Bounds (a, b, c, d, e, f, g, h)) size size

-- ...
