module Main where
import Cantor

main =
  putStrLn (show ((enumerate :: [(Int, Bool, Int, Char)]) !! 100000001))
