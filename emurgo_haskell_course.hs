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



---- Custom Data Types ----
bar :: Int
bar = 10

fun :: Int -> Int
fun x = x * 10

-- Create our own data types. The item before the equals are type constructors. The items after the equals are data constructors. 
data Mood = Sad | Happy | Angry deriving (Show)

sad :: Mood
sad = Sad

hap :: Mood
hap = Happy


data Temperature = C Float | F Float deriving Eq 

-- In this case, BinaryTree is given as a second type for Node. Meaning that it is recursive. 
data BinaryTree = Leaf Int | Node Int BinaryTree deriving Show

myTree :: BinaryTree
-- In this example you can see the recursion of the type definition which allows you to continue passing values. 
myTree = Node 10 (Node 11 (Leaf 12))

data Student = Student String Int Float deriving Show
auxName :: Student -> String
auxName (Student x y z) = x
john = Student "John" 17 9
johnName = auxName john 

-- Structure notation gives auxiliary functions to map and extract values. 
data Student' = Student' {
name' :: String,
age :: Int,
grade :: Float
} deriving Show

tucker = Student' "Tucker" 29 12
tuckerGrade = grade tucker

-- Blockchain
type CBHash = Int

data Blockchain = Genesis CBHash | Block CBHash Blockchain deriving Show 
gen = Genesis 0
 
firstSt  = gen
secondSt = Block 1 firstSt 
thirdSt  = Block 2 secondSt
fourthSt  = Block 3 thirdSt
fifthSt  = Block 4 (Block 3 (Block 2 (Block 1 (Genesis 0))))


-- An example of a type alias
type ComplicatedTypeDef= Int-> Float -> Float-> Char-> Integer -> Bool
myCompDataType :: ComplicatedTypeDef
myCompDataType a b c d e = True

type Pos = (Int, Int)
type StateChange = Pos -> Pos

data Move = North | South | East | West

move :: Move -> StateChange
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1, y)
move West (x,y) = (x-1, y)


-- How currying works within a function
-- curringEx (Int Int Int) (Int)
-- (Int Int) (Int)
-- (Int) (Int)

---- Recursion ----
numList :: [Int]
numList = [1..22]

length' :: [a] -> Int
length' [] = 0
length' list = 1 + length (tail list)

findLength = length' numList
findStrLength = length' "Hello"

length'' :: [a] -> Int
length'' [] = 0
length'' (x:xs) = 1 + length'' xs

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Special pattern to split lists
specialSum :: [Int] -> Int
specialSum [] = 0
specialSum [x] = x
-- specialSum 
specialSum (x:y:zs) = y

-- Create a function that takes the sum of every two numbers in a list
sumOf2 :: [Int] -> [Int]
sumOf2 [] = []
sumOf2 [x] = [x]
sumOf2 (x:y:zs) = x + y : sumOf2 zs

example1 = sumOf2 [1]
example2 = sumOf2 [1,2]
example3 = sumOf2 [1..50]


-- Concatenation operators
-- a : [a]
-- [a] ++ [a]
listOf10 = (:) 1 [2..10]
listOf20 = (++) [1..10][2..20]
oneList = (:) 1 []


-- Recursive function to add two to all
-- (Int->Int) in this case is a function
applyToAll :: (Int -> Int) -> [Int] -> [Int]
applyToAll _ [] = []
applyToAll f (x:xs) = (f x) : applyToAll f xs

addTwo = applyToAll (+2) [2,3,4,5]

multiplyByTwo = applyToAll (*2) [2,3,4,5]

-- When you see the type definition like (Int -> Int) it means it is expecting a function to be passed in. 