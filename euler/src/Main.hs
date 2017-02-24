module Main where

import System.Environment

main :: IO ()
main = do
  arg <- fmap head getArgs 
  let answer = show (
                      case arg of "1" -> euler1
                                  "2" -> euler2
                    ) 
  putStrLn answer
  return ()

euler1 :: Int
euler1 = sum ( filter (\x -> mod x 3 == 0 || mod x 5 == 0) [1..999] )

nextFib :: (Int, Int) -> (Int, Int)
nextFib (a,b) = (a + b, a)


fib :: [Int]
fib = fmap fst (iterate nextFib (1,0))

euler2 :: Int
euler2 = sum ( filter (even) (takeWhile (\x ->  x < 4000000) fib))

