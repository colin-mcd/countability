{-# LANGUAGE FlexibleInstances #-}

module Cantor where
import Data.Maybe (fromJust)
--import qualified GHC.Real
--import Stream

type Stream a = [a]

newtype Bounds a = Bounds (Maybe Integer)
unbounded = Bounds Nothing
bounded = Bounds . Just

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

-- Yields all possible lists with elements of a stream
star :: Stream a -> Stream [a]
star xs = undefined
--star xs = [] : map (\(y, ys) -> y : ys) (diagonalize (xs, star xs))
--star xs = map (\(y, ys) -> y : ys) (diagonalize (xs, [] : star xs))

unstar :: Countable a => [a] -> Integer
unstar as = undefined
--unstar [] = 0
--unstar (a : as) = TODO
--unstar [a] = 1 + toIndex a
--unstar (a : as) = 1 + undiagonalize (toIndex a) (unstar as)

alternate :: [a] -> [a] -> [a]
alternate (a : as) bs = a : alternate bs as
--alternate [] bs = bs

eithernate :: Stream a -> Stream b -> Stream (Either a b)
eithernate as bs = alternate (fmap Left as) (fmap Right bs)

deriveBoundedEnumerate :: (Bounded a, Enum a) => [a]
deriveBoundedEnumerate = [minBound..maxBound]

deriveBoundedIndex :: Enum a => a -> Integer
deriveBoundedIndex = toInteger . fromEnum

indexAsBound :: Enum a => a -> Bounds a
indexAsBound = bounded . succ . toInteger . fromEnum

deriveBoundedSize :: (Bounded a, Enum a) => Bounds a
deriveBoundedSize = indexAsBound maxBound -- - fromEnum minBound

zplus :: (Enum n, Num n) => [n]
zplus = [1, 2..] -- strictly positive integers
zminus :: (Enum n, Num n) => [n]
zminus = [-1, -2..] -- strictly negative integers

--instance (Enum a, Bounded a) => Countable a where
--  enumerate = deriveBoundedEnumerate
--  toIndex = fromEnum
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
  enumerate = star enumerate -- TODO: case for bounded a
  toIndex = unstar -- TODO: case for bounded a
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
