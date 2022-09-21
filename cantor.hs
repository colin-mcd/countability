{-# LANGUAGE FlexibleInstances #-}

module Cantor where
--import qualified GHC.Real
--import Stream

type Stream a = [a]

class Countable a where
  enumerate :: Stream a

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

--Powerset of a potentially infinite-length list
--powerset :: Stream a -> Stream [a]
powerset xs =
  [] : foldr
    (\ x next acc ->
       let acc' = map (\ xs -> xs ++ [x]) acc in
         acc' ++ (next (acc ++ acc')))
    (\ _ -> []) xs [[]]

--permutations :: Stream a -> Stream [a]
permutations xs =
  [] : foldr
    (\ x next acc ->
       let acc' = concatMap (insertions x) acc in
         acc' ++ (next (acc ++ acc')))
    (\ _ -> []) xs [[]]

--Returns a list (of lists) of all the places we can insert an element into a list
--insertions :: a -> [a] -> [[a]]
insertions a as = insertionsh a as [] where
  insertionsh :: a -> [a] -> [a] -> [[a]]
  insertionsh a [] bs' = [reverse (a : bs')]
  insertionsh a (b : bs) bs' =
    (reverse bs' ++ a : b : bs) : insertionsh a bs (b : bs')

--alternate :: [a] -> [a] -> [a]
alternate (a : as) bs = a : alternate bs as
alternate [] bs = bs

deriveBoundedEnumerate :: (Bounded a, Enum a) => [a]
deriveBoundedEnumerate = [minBound..maxBound]

instance Countable Bool where
  enumerate = deriveBoundedEnumerate
instance Countable () where
  enumerate = deriveBoundedEnumerate
instance Countable Char where
  enumerate = deriveBoundedEnumerate
instance Countable Word where
  enumerate = deriveBoundedEnumerate
instance Countable Ordering where
  enumerate = deriveBoundedEnumerate
instance Countable Int where
  enumerate = [0..]
instance Countable Integer where
  enumerate = [0..]
instance Countable a => Countable [a] where
  enumerate = permutations enumerate
instance (Countable a, Countable b) => Countable (Either a b) where
  enumerate = alternate (fmap Left enumerate) (fmap Right enumerate)
instance Countable a => Countable (Maybe a) where
  enumerate = Nothing : fmap Just enumerate
instance (Countable a, Countable b) => Countable (a, b) where
  enumerate = diagonalize (enumerate, enumerate)
instance (Countable a, Countable b, Countable c) => Countable (a, b, c) where
  enumerate = fmap (\ ((a, b), c) -> (a, b, c)) (diagonalize (diagonalize (enumerate, enumerate), enumerate))
instance (Countable a, Countable b, Countable c, Countable d) => Countable (a, b, c, d) where
  enumerate = fmap (\ (((a, b), c), d) -> (a, b, c, d)) (diagonalize (diagonalize (diagonalize (enumerate, enumerate), enumerate), enumerate))
instance (Countable a, Countable b, Countable c, Countable d, Countable e) => Countable (a, b, c, d, e) where
  enumerate = fmap (\ ((((a, b), c), d), e) -> (a, b, c, d, e)) (diagonalize (diagonalize (diagonalize (diagonalize (enumerate, enumerate), enumerate), enumerate), enumerate))
instance (Countable a, Countable b, Countable c, Countable d, Countable e, Countable f) => Countable (a, b, c, d, e, f) where
  enumerate = fmap (\ (((((a, b), c), d), e), f) -> (a, b, c, d, e, f)) (diagonalize (diagonalize (diagonalize (diagonalize (diagonalize (enumerate, enumerate), enumerate), enumerate), enumerate), enumerate))
instance (Countable a, Countable b, Countable c, Countable d, Countable e, Countable f, Countable g) => Countable (a, b, c, d, e, f, g) where
  enumerate = fmap (\ ((((((a, b), c), d), e), f), g) -> (a, b, c, d, e, f, g)) (diagonalize (diagonalize (diagonalize (diagonalize (diagonalize (diagonalize (enumerate, enumerate), enumerate), enumerate), enumerate), enumerate), enumerate))
instance (Countable a, Countable b, Countable c, Countable d, Countable e, Countable f, Countable g, Countable h) => Countable (a, b, c, d, e, f, g, h) where
  enumerate = fmap (\ (((((((a, b), c), d), e), f), g), h) -> (a, b, c, d, e, f, g, h)) (diagonalize (diagonalize (diagonalize (diagonalize (diagonalize (diagonalize (diagonalize (enumerate, enumerate), enumerate), enumerate), enumerate), enumerate), enumerate), enumerate))
-- ...
instance (Countable a, Countable b, Countable c, Countable d, Countable e, Countable f, Countable g, Countable h, Countable i, Countable j, Countable k, Countable l, Countable m, Countable n, Countable o) => Countable (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  enumerate = fmap (\ ((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o))
    (diagonalize (diagonalize (diagonalize (diagonalize (diagonalize (diagonalize (diagonalize (diagonalize (diagonalize (diagonalize (diagonalize (diagonalize (diagonalize (diagonalize (enumerate, enumerate), enumerate), enumerate), enumerate), enumerate), enumerate), enumerate), enumerate), enumerate), enumerate), enumerate), enumerate), enumerate), enumerate))

--instance Countable (GHC.Real.Ratio Integer) where
--  enumerate = fmap (uncurry (GHC.Real.:%)) enumerate
