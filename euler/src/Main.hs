module Main where

import System.Environment

main :: IO ()
main = do
  arg <- fmap head getArgs 
  let answer = show (
                      case arg of "1" -> euler1
                    ) 
  putStrLn answer
  return ()

euler1 :: Int
euler1 = sum ( filter (\x -> mod x 3 == 0 || mod x 5 == 0) [1..999] )
