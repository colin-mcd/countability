module Derivations where
import Cantor

data BinTree a = Leaf a | Node (BinTree a) (BinTree a) deriving Show

instance Countable a => Countable (BinTree a) where
  enumerate =
    fmap (either Leaf (uncurry Node)) enumerate

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

instance Countable Regex where
  enumerate =
    fmap (either (\ () -> ReEmpty) $
          either (\ () -> ReEps) $
          either ReSym $
          either (uncurry ReCat) $
          either (uncurry ReOr) $
          ReStar) enumerate
