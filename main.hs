module Main where
import Cantor

main =
  putStrLn (show ((enumerate :: [(Integer, Bool, Integer, Char)]) !! 1000001))
--  >> putStrLn (show (toIndex (0 :: Integer, True, 13 :: Integer, '\1010')))
