module Main where
import Cantor

main =
  lines <$> getContents >>=
  foldr (\ln next -> putStrLn (ln ++ " is index " ++ show (toIndex ln)) >> next) (return ())
--  putStrLn (show ((enumerate :: [(Integer, Bool, Integer, Char)]) !! 1000001)) >>
--  putStrLn (show (toIndex (1 :: Integer, False, 13 :: Integer, '\1010')))
