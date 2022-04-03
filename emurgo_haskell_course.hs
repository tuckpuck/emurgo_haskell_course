-- This is used to name this in the repl
module Emurgo_Haskell_Course where

-- Types
a = 33.33      --Double
b = 'R'        --Char
c = "Roberto"  --[Char]
d = 10         --Integer
e = (2, "Tucker", 'C', 3.4)--(Integer, [Char], Char, Double)
f = True       --Bool
g = [1,2,3,4]  --[Integer]


-- For single character use 'c', for double character use "string"
-- Lists are not linked lists or arrays, though similar to dynamic arrays. Syntax can be deconstructed as 2:3:4:[]

-- :l load
-- :r refresh
-- :t type
-- :? to see all commands
-- :info to check type classes

-- Type definition
y :: Int -> Int
-- Function definition
y x = x + 1

-- Assign result to a variable
z = y 10

-- 'Num a' is a type restriction. a is a type variable. The variables can be any type of number, but they all must be the same type
plus :: Num a => a -> a -> a
plus x y = x + y

-- These are all the same
name :: String
name = "Tucker"

name2 :: [Char]
name2 = ['T','u','c','k','e','r']

name3 = 'T':'u':'c':'k':'e':'r':[]

-- Int and Integer are different types. Always respect the types.

-- Recursive functions
factorial :: Integer -> Integer
-- Exit condition
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Rewrite using guards
factorial' :: Integer -> Integer
factorial' n
  | n == 0 = 1
  | otherwise = n * factorial' (n - 1)

-- Rewrite using case
factorial'' :: Integer -> Integer
factorial'' n = case (n > 0) of  
    False -> 1
    True -> n * factorial'' (n - 1) 

-- Rewrite using if else
factorial''' :: Integer -> Integer
factorial''' n = if n <= 0 then (1) else n * factorial''' (n - 1)

-- Rewrite in case of negative numbers using if else
safeFactorial :: Integer -> Integer
safeFactorial 0 = 1
safeFactorial n = if n < 0 then (-1) else n * safeFactorial (n - 1)

-- List functions
headtail = 2:[3,4,5,6,7,8]
he = head headtail
ta = tail headtail

-- A pattern we can use. X is the head, XS is equal to the rest. 
(x:xs) = [1,2,3,4,5]

listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

