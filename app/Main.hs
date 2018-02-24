module Main where

import Data.Char (ord, chr)

main :: IO ()
main = do
  putStrLn "Hello World!"
  putStrLn "bye"

max' :: Ord a => a -> a -> a
max' a b = undefined

maximum' :: Ord a => [a] -> a
maximum' []     = error "why?"
maximum' [x]    = undefined
maximum' (x:xs) = undefined

maximumTotal :: Ord a => [a] -> Maybe a
maximumTotal []     = undefined
maximumTotal [x]    = undefined
maximumTotal (x:xs) = undefined

data Tree = Leaf
    | Node Tree Tree
    deriving (Show, Eq)

longestBranch :: Tree -> Int
longestBranch Leaf         = undefined
longestBranch (Node b1 b2) = undefined

shortestBranch :: Tree -> Int
shortestBranch Leaf         = undefined
shortestBranch (Node b1 b2) = undefined

-- how many pattern matches to use?
isSubtree :: Tree -> Tree -> Bool
isSubtree = undefined

factorial :: Integer -> Integer
factorial n = undefined

isPalindrome :: String -> Bool
isPalindrome s = undefined

isPalindromeInt :: Int -> Bool
isPalindromeInt n = undefined

-- try pointfree notation
sumOdd :: [Int] -> Int
sumOdd = undefined

-- use ord, chr, `div`, `mod`
rot13 :: String -> String
rot13 = undefined

-- use list comprehension
rightTriangles :: Int -> [(Int, Int, Int)]
rightTriangles n = undefined

safeHead :: [a] -> Maybe a
safeHead _ = undefined

isHeadEq5 :: [Int] -> Maybe Bool
isHeadEq5 l =
  case safeHead l of
    Just 5 -> Just True
    Just _ -> Just False
    _      -> Nothing

-- write isHeadEq5 using monadic Maybe
isHeadEq5M :: [Int] -> Maybe Bool
isHeadEq5M l = do
  return undefined
