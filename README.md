# Console exercises


Try in the ghci:
```
 ghci> :help
 ghci> :h
 ghci> :?
```

Pay attention to:
```
:load (:l)
:reload (:r)
:type (:t)
:info (:i)
:quit (:q)
```

Try:
```
 ghci> 2
 ghci> :t 3
 ghci> :t True
 ghci> :i Bool
 ghci> :t (True, "hello")
 ghci> 2 + 2
 ghci> 2 + 3 * 5 + 1.4 / (2 * pi)
 ghci> 5 `div` 2
 ghci> div 5 2
 ghci> 5 `mod` 2
 ghci> mod 5 2
 ghci> True && False
 ghci> True || False
 ghci> True && 1
 ghci> True /= False
 ghci> not (3 >= 1)
 ghci> not 3 >= 1
 ghci> (3 > 2) && (2 > 1)
 ghci> 3 > 2 && 2 > 1
```

```
 ghci> let p =  3.14
 ghci> p
 ghci> let r = 8
 ghci> let c = 2 * p * r
 ghci> c
```

```
 ghci> :show [press TAB]
 ghci> :show bindings
 ghci> :show modules
```

Create file HelloWorld.hs
```
module Main where

main = putStrLn "Hello world!"
```

```
 ghci> :l HelloWorld.hs
```

Edit file creating content:
```
module Main where

main :: IO ()
main = do
  putStrLn "Hello world!"
  putStrLn "bye!'
```

```
 ghci> :r
```

Create file Examples.hs
```
module Examples where

absInt :: Int -> Int
absInt n | n > 0 = n
        | n < 0 = -n
```

Test it in console:
```
 ghci> :l Examples.hs
 ghci> absInt 1
 ghci> absInt (-1)
 ghci> absInt 0
```

Fix the program.

Add to Examples.hs:
```
addTwoTuple :: Num a => (a, a) -> a
addTwoTuple (x, y) = x + y

addTwoCurried :: Num a => a -> a -> a
addTwoCurried x y = x + y
```

Test:
```
ghci> addTwoTuple (1,2)
ghci> addTwoCurried (1,2)
ghci> addTwoCurried 1 2
ghci> addTwoTuple 1 2
ghci> :t curry
ghci> :t uncurry
ghci> :t curry addTwoTuple
ghci> :t uncurry addTwoCurried
ghci> (uncurry addTwoCurried) (1,2)
ghci> (curry addTwoTuple) 1 2
```

Sections:
```
ghci> :t 2 * 3
ghci> :t (*)
ghci> :t (2 *)
ghci> :t (* 2)
```

Lazzyness:
```
ghci> let fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]
ghci> head fibs
ghci> take 10 fibs
ghci> :sp fibs
```

List operations:
```
ghci> import Data.List
ghci> let xs = [1..7]
ghci> xs
ghci> length xs
ghci> reverse xs
ghci> head xs
ghci> tail xs
ghci> last xs
ghci> init xs
ghci> take 2 xs
ghci> drop 2 xs
ghci> 0 : xs
ghci> xs ++ [9]
ghci> import Data.Monoid
ghci> xs <> [9]
ghci> xs !! 2
ghci> null xs
ghci> any (> 2) xs
ghci> all (> 0) xs
ghci> zip xs ['a','b']
ghci> splitAt 2 xs
ghci> sort [8,3,9,2,5]
ghci> 2 `elem` xs
ghci> elem 2 xs
ghci> minimum xs
ghci> maximum xs
ghci> sum xs
ghci> product xs
```

List comprehensions:
```
ghci> [x^3 | x <- [1..5]]
ghci> [(i,j) | i <- [0..3], j <- [0..i]]
```

Lambda expressions:
```
ghci> :t (\x -> \y -> x + y)
ghci> :t (\x y -> x + y)
ghci> :t (\x y -> x + y) 1
ghci> :t (\x y -> x + y) 1 2 -- porównujemy wynik z poprzednim
ghci> let addTwo = \x -> \y -> x + y
ghci> :t addTwo
ghci> addTwo 1 2
```

Higher order functions:
```
ghci> :t (.)
ghci> :t flip
ghci> :t ($)
ghci> :t const
ghci> :t id
```

Map:
```
ghci> map (*2) [1..5]
ghci> map (^2) [1..10]
ghci> import Data.Char
ghci> map toUpper "hello world"
ghci> filter (/= 'l') "hello world"
ghci> foldl (+) 0 [1..4]
ghci> let bList = [True, True, False, True]
ghci> foldl (||) False bList
ghci> foldl (&&) True bList
```

IO:
```
ghci> :t putStrLn
ghci> putStrLn "Hello world!"
ghci> :t getLine
ghci> :t print
ghci> print 1
ghci> print [1..7]
ghci> print (1,2,3)
```

Functor:
```
ghci> :i Functor
ghci> :t fmap (* 2)
ghci> fmap (* 2) Nothing
ghci> fmap (* 2) (Just 3)
ghci> fmap (* 2) (Left 3)
ghci> fmap (* 2) (Right 3)
ghci> fmap (* 2) [1..5]
ghci> :i Maybe
ghci> :i Either
ghci> 0 <$ Left 3
ghci> 0 <$ Right 3
ghci> 'a' <$ [1..5]
ghci> 4 <$ "hello"
ghci> 4 <$ Nothing
ghci> 4 <$ Just 7
ghci> (+4) <$> Just 7
```

Applicative:
```
ghci> (+) <$> Just 2 <*> Just 3
ghci> pure (+) <*> Just 2 <*> Just 3
ghci> :t pure
ghci> :t return
ghci> pure 1 :: Either a Int
ghci> return 1 :: Either a Int
ghci> pure (+1) <*> Left 0
ghci> pure (+1) <*> Right 0
ghci> Right (+1) <*> Right 0
ghci> (*) <$> [1,2,3] <*> [10,11,12]
ghci> (++) <$> Just "Hello " <*> Just "world!"
ghci> (++) <$> Just "Hello " <*> Nothing
ghci> (++) <$> Nothing <*> Just "world!"
```