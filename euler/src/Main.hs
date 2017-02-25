module Main where


-- https://projecteuler.net/problem=1
-- http://www.mathblog.dk/project-euler-problem-1

import System.Environment
import Data.List
import Data.Maybe
import Debug.Trace

main :: IO ()
main = do
  arg <- fmap head getArgs 
  let answer =
                case arg of "1" -> show euler1
                            "2" -> show euler2
                            "3" -> show euler3
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




euler3 :: Integer
euler3 = euler3Recursive (1, 2, 600851475143)


-- if remainder / i is a whole number
-- then i is a factor of remainder, 
-- since i starts at 2, if i divides
-- remainder, it must also be prime
euler3Recursive :: (Integer, Integer, Integer) -> Integer
euler3Recursive (maxPrime, i, remainder) = 
    if( (mod remainder i) == 0) 
        then euler3Recursive (i, i, quot remainder i)
        else if i > remainder 
                then maxPrime
                else euler3Recursive (maxPrime, i+1, remainder)
   



