module Derivations where
import Cantor

-- Instances for bounded types are super easy:
instance Countable Word where
  enumerate = deriveBoundedEnumerate
  toIndex = deriveBoundedIndex
  size = deriveBoundedSize

instance Countable Ordering where
  enumerate = deriveBoundedEnumerate
  toIndex = deriveBoundedIndex
  size = deriveBoundedSize

data Color = Red | Orange | Yellow | Green | Blue | Indigo | Violet deriving (Bounded, Enum)
instance Countable Color where
  enumerate = deriveBoundedEnumerate
  toIndex = deriveBoundedIndex
  size = deriveBoundedSize

data Size = Small | Medium | Large deriving (Bounded, Enum)
instance Countable Size where
  enumerate = deriveBoundedEnumerate
  toIndex = deriveBoundedIndex
  size = deriveBoundedSize

-- Composite bounded datatype: map TShirt c s to the product (c, s),
-- using builtin instances for product types
data TShirt = TShirt Color Size
instance Countable TShirt where
  enumerate = map (\(c, s) -> TShirt c s) enumerate
  toIndex (TShirt c s) = toIndex (c, s)
  size = let Bounds b = size :: Bounds (Color, Size) in Bounds b


-- Recursive types seem a little more involved,
-- but they're really just a straightforward folding/unfolding
-- from the rec type to sums and products and vice-versa:
data BinTree a = Leaf a | Node (BinTree a) (BinTree a) deriving Show

foldBinTree :: Either a (BinTree a, BinTree a) -> BinTree a
foldBinTree = either Leaf (uncurry Node)

unfoldBinTree :: BinTree a -> Either a (BinTree a, BinTree a)
unfoldBinTree (Leaf a) = Left a
unfoldBinTree (Node l r) = Right (l, r)

instance Countable a => Countable (BinTree a) where
  enumerate = map foldBinTree enumerate
  toIndex = toIndex . unfoldBinTree
  size = unbounded

data Regex =
    ReEmpty
  | ReEps
  | ReSym Char
  | ReCat Regex Regex
  | ReOr Regex Regex
  | ReStar Regex

instance Show Regex where
  show ReEmpty = "ø"
  show ReEps = "ε"
  show (ReSym c) = [c]
  show (ReCat r1 r2) = "(" ++ show r1 ++ show r2 ++ ")"
  show (ReOr r1 r2) = "(" ++ show r1 ++ "|" ++ show r2 ++ ")"
  show (ReStar r) = show r ++ "*"

foldRegex :: Either () (Either () (Either Char (Either (Regex, Regex) (Either (Regex, Regex) Regex)))) -> Regex
foldRegex =
  either (\ () -> ReEmpty) $
  either (\ () -> ReEps) $
  either ReSym $
  either (uncurry ReCat) $
  either (uncurry ReOr) $
  ReStar
unfoldRegex :: Regex -> Either () (Either () (Either Char (Either (Regex, Regex) (Either (Regex, Regex) Regex))))
unfoldRegex ReEmpty = Left ()
unfoldRegex ReEps = Right (Left ())
unfoldRegex (ReSym c) = Right (Right (Left c))
unfoldRegex (ReCat r1 r2) = Right (Right (Right (Left (r1, r2))))
unfoldRegex (ReOr r1 r2) = Right (Right (Right (Right (Left (r1, r2)))))
unfoldRegex (ReStar r) = Right (Right (Right (Right (Right r))))

instance Countable Regex where
  enumerate = map foldRegex enumerate
  toIndex = toIndex . unfoldRegex
  size = unbounded
