module Main where


-- https://projecteuler.net/problem=1
-- http://www.mathblog.dk/project-euler-problem-1

import System.Environment
import Data.List
import Data.Maybe
import Debug.Trace
import System.CPUTime
import Text.Printf
import Constants

main :: IO ()
main = do
  arg <- fmap head getArgs 
  start <- getCPUTime 
  let answer =
                case arg of "1" -> show euler1
                            "2" -> show euler2
                            "3" -> show euler3
                            "4" -> show euler4
                            "5" -> show euler5
                            "6" -> show euler6
                            "7" -> show euler7
                            "8" -> show euler8
                            "9" -> show euler9
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "Computation time: %0.3f sec\n" (diff :: Double)
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

isPrimeSlow :: Integer -> Bool
isPrimeSlow 1 = False
isPrimeSlow x = isPrimeSlowRecursive 2 x

isPrimeSlowRecursive :: Integer -> Integer -> Bool
isPrimeSlowRecursive acc x
    | acc * acc > x = True
    | x `mod` acc == 0 = False
    | otherwise = isPrimeSlowRecursive (1+acc) x


euler7 :: Integer
euler7 = last (take 10001 [x | x <- [1 .. ], isPrimeSlow x])

e8SubLists :: [[Integer]]
e8SubLists =  scanl fun (take 13 e8Ints) (drop 13 e8Ints) where
    fun xs x = (tail xs)  ++ [x]

euler8 :: Integer
euler8  = maximum (fmap product e8SubLists) 

euler9 :: Integer
euler9 = head [x | 
    a <- [1 .. 100],  
    b <- [a .. 100],  
    let c = round (sqrt( fromIntegral (a^2 + b^2))),  
    a + b + c == 1000, 
    (a^2) + (b^2) == c^2, 
    let x = a * b * c ]




