{-# LANGUAGE OverloadedStrings #-}

import Emurgo_Haskell_Course (y'', xs, y)
import Data.Text
import Data.List
import Data.Char



-- Higher order functions
-- Every function takes more than one argument returns a function in the result

add :: Int -> Int -> Int
add x y = x + y

add' :: Int -> Int -> Int
add' x = \y -> x + y

-- Higher order function is a function which takes a function in the argument

applyFunctionTwice :: (a -> a) -> a -> a
applyFunctionTwice f x = f (f x)

twice1 = applyFunctionTwice (^2) 3
twice2 = applyFunctionTwice Data.List.reverse "hello"


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

upper = Data.Text.pack "ejsdsallo"
map = map' (+10) [12,34,65]
map2 = map' Data.Text.toUpper [upper]
map3 = map' not [True, False, False]

-- Filter
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' _ [] = []
filter'' f (x:xs) = if (f x)  then (x: filter'' f xs) else (filter'' f xs)

filter1 = filter'' (>7) [1..10]
filter2 = filter'' even [2,3,4,5,6,7,8]
filter3 = filter'' (/= ' ') "Hello there"

-- Fold
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ i [] = i
foldr' f i (x:xs) = x `f` foldr' f i xs

sumIt = foldr' (+) 20
sumEx = sumIt [20, 34, 53]

-- Application operator
-- $ takes a function and a value and produces the result of the application of the function to the value
-- Lowest priority among operators

tail1 = Data.Text.tail(Data.Text.tail ("world"))
tail2 = Data.Text.tail $ Data.Text.tail (Data.Text.pack "world")


-- Composition operator
-- . takes two functions and produces the function which is equal to the composition of the functions
compo = (^2) . (*3) . (+4) $ 7
compo2 = Data.Char.toUpper . Prelude.head $ "hello"