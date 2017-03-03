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
                            "4" -> show euler4
                            "5" -> show euler5
                            "6" -> show euler6
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
   


euler4 :: Integer
euler4 = maximum (filter  
            (\x -> (show x) == (reverse (show x)))  
            [ x * y | x <-  [100 .. 999], y <- [100 .. 999] ] )


factor :: Integer -> [Integer]
factor n = factorRecursive 2 n 


factorRecursive ::  Integer -> Integer -> [Integer]
factorRecursive i x  
   |  x `mod` i == 0 =  i : (factorRecursive i (toInteger(div x i)))
   |  i * i > x = [x]
   |  otherwise = factorRecursive (i+1) x

allFactorsRecursive :: [Integer] -> [Integer] -> [Integer]
allFactorsRecursive  remaining  acc
    | remaining == [] = [x | x <- acc, x /= 1]
    | otherwise = allFactorsRecursive (tail remaining) (acc ++ (factor ( divAll  (head remaining) acc)))

-- returns the remainder after dividing x by all elements of xs that divide x evenly
divAll :: Integer -> [Integer] -> Integer
divAll x xs 
   | x == 1 = 1
   | xs == [] = x
   | x `mod` (head xs) == 0 = divAll (x `div` (head xs)) (tail xs)
   | otherwise =  divAll x (tail xs)
   
euler5 :: Integer
euler5  = product ( allFactorsRecursive [1 .. 20] [] )


euler6 :: Integer
euler6 = ((sum  [(1 :: Integer) .. 100]) ^ 2)  -  (sum (fmap (^ 2) [1 .. 100]))   
