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

-- Reduction or evaluation is where the operations are calculated down to a single value in order. 

-- Array concatenation operators
-- a : [a]
-- [a] ++ [a] 

-- Anonymous functions can be written instead of defined functions, for flexability, when a function will only be used once
increment :: Int -> Int
increment x = (\x -> x + 1) x
testIncrement = increment 10 

-- Factorials
factorial'''' :: Int -> Int
factorial'''' 0 = 1
factorial'''' x = x * factorial'''' (x - 1)

-- Same as above, written with guards
factorial''''' :: Int -> Int
factorial''''' n | n == 0 = 1
  | otherwise = n * factorial''''' (n-1)

-- Setting up a custom data type
data Health = Healthy | Sick deriving (Show)
data Temperature' = C' Int deriving (Show)
temps :: [Temperature']
temps = [C' 36, C' 37, C' 38, C' 39, C' 40]

-- Make a recursive function using two custom data types
areYouSick :: [Temperature] -> [Health]
areYouSick [] = []
areYouSick ((C y):xs) = (if y >= 35 && y <= 36 
                         then Healthy 
                         else Sick) : areYouSick xs

data Grades = Grade Int Char deriving Show

grades :: [Grades]
grades = [Grade 65 'D', Grade 72 'C' , Grade 84 'B', Grade 96 'A']

calcGPA :: [Grades] -> Int
calcGPA [] = 0
-- Lst@ is notation for the whole provided list
calcGPA lst = (sumGrades lst) `div` (length lst)

sumGrades :: [Grades] -> Int
sumGrades [] = 0
sumGrades ((Grade num ltr):xs) = num + sumGrades xs

-- Where implies that everything after the where will be within that scope. 



-- Execute transformations in a credit card number
checkDigit :: [Int] -> Int
checkDigit [x] = x
checkDigit (x:xs) = checkDigit(xs)

dropLast :: [Int] -> [Int]
dropLast [x] = []
dropLast (x:xs) = x : dropLast xs

reverse' :: [Int] -> [Int]
reverse' [] = [] 
reverse' (x:xs) = reverse' xs ++ [x]

doubleEachOdd :: [Int] -> [Int]
doubleEachOdd [] = [] 
doubleEachOdd [x] = [x*2] 
doubleEachOdd (x:y:zs) = x*2:y:doubleEachOdd zs 

subtract9 :: [Int] -> [Int]
subtract9 [] = []
subtract9 (x:xs) 
  | x > 9 = x - 9 : subtract9 xs
  | otherwise = x : subtract9 xs


cc = [6,0,3,3,9,5,2,0,5,6,3,3,4,1,6,9]
step1 = dropLast cc
step2 = reverse' step1
step3 = doubleEachOdd step2
step4 = subtract9 step3


-- List aux functions to know
-- head, tail, take, map, filter, zip, takeWhile, !!, elem
-- !! acts as an index 
-- best practice is often to create a safe version of the function
-- ex. native head function does not handle empty list [] as an input
-- Instead it will throw an exception.
-- A total function will handle all cases


-- Higher order functions are functions that take a function as an input
-- To show this is a function in the type signature, you should add (), ex. (Int -> Int)
twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)
testTwice = twice (+3) 4

divideBy :: Int -> Int -> Int
divideBy a b = a `div` b

performOp :: (Int -> Int) -> Int -> Int -> Int
performOp f x y = y * (f x)

map' :: (a -> b) -> [a] -> [b]
map' _ [] = [] 
map' f (x:xs) = f x : map f xs 
mapNums = map' (+10) [1,2,3,4]
mapStrings = map' reverse ["hello", "good morning"]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs 

sum'' :: [Int] -> Int
sum'' [] = 0
sum'' (x:xs) = x + sum xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b 
myFoldr _ nil [] = nil
myFoldr f nil (x:xs) = f x (myFoldr f nil xs)

-- This is an example of creating a partial function using currying
sum''' = myFoldr (+) 0
testSum = sum''' [1,2,3]

-- Let notation
aaa x = let y = x + 3
            z = x + 2
        in y + z 
testAaa = aaa 100

--  Where notation
mathFunctionWhere :: Int -> Int -> Int -> Int
mathFunctionWhere a b c = diff1 + diff2 + prod + a
  where
    diff1 = c - a
    diff2 = b - a
    prod = a * b * c

testWhere = mathFunctionWhere 10 20 30

-- HW1

--1. Fix the Types -- Asume that the functions implementations are CORRECT, just fix the type definition needed for them to run, feel free to test on the REPL
fixMe1 :: Integer -> Integer -> Bool   
fixMe1 n m
  | n >= m    = True
  | otherwise = True

fixMe2 :: Double -> Double -> String     
fixMe2 n m = if n / m > 10  then "A-F" else "0-9"

abc = [1..11]
fixMe3 :: [Integer] -> Integer
fixMe3 [] = 0
fixMe3 (x:xs) = x + fixMe3 xs

fixMe4 ::  [Char] -> [Char] -> String
fixMe4 fName lName = lName ++ ", " ++ fName


--Recursion and Polymorphism
scoresT :: Fractional a => [a]
scoresT = [74,85,81,98,69.99,78,87,93]

myDlength :: Fractional a => [a] -> a
myDlength [] = 0
myDlength (x:xs) = 1 + myDlength (xs)  

fixMe5 :: (Ord a,Fractional a) => ([a] -> a) -> ([a] -> a) -> [a] -> Char
fixMe5 myOper myLen lst
  | (myOper lst)/(myLen lst)  > 90.99   = 'A'
  | (myOper lst)/(myLen lst)  > 80.99   = 'B'
  | (myOper lst)/(myLen lst)  > 70.99   = 'C'
  | otherwise                           = 'F'
-- Test it on your repl:
-- fixMe5 sum myDlength scoresT
-- The answer should be 'B'


-- 2. Define the base case
beginsWith :: Char -> String -> Bool
beginsWith _ [] = False
beginsWith c (x:xs)   = c == x


{- 3. Create a function all' that takes 2 inputs, (1st) a comparison operation(>,>=,<,<=,==) to any member of (2nd) a list,
--    and output True if all elements of the functions comply, False if any of the elemnts doesn't, WITHOUT using map
-}
-- Original answer
-- all' :: (Ord a) => (a -> a -> Bool) -> [a] -> Bool
-- all' _ [] = False
-- all' _ [_] = False
-- all' comp (x:y:xs) = x `comp` y
-- Can't fully figure this one out 

all' :: (Ord a) => (a -> Bool) -> [a] -> Bool
all' p  [] = True
all' p (x:xs)
  | p x = all' p xs
  | otherwise = False


-- Given a Word and a letter (case sensitive), create function that count the number of times that letter shows up in the word
countHowManyTimesLetterIsInWord :: Char -> [Char] -> Int
countHowManyTimesLetterIsInWord _ [] = 0
countHowManyTimesLetterIsInWord c st = length (filter (==c) st)

myLtrCount :: String -> Char -> Int
myLtrCount [] _ = 0
myLtrCount (x:xs) ltr
  | x == ltr = 1 + (myLtrCount xs ltr)
  | otherwise = 0 + (myLtrCount xs ltr)


myLength = myFoldr (\ x r -> 1 + r) 0

myMap :: (a -> b) -> [a] -> [b]
myMap f list = foldr (\x xs -> f x : xs) [] list

-- id is the identity function.
-- It is a function that always returns the value that was used as its argument, unchanged
-- In this case is used to mean 'dont do anything and continue evaluating'

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f list = foldr (\x -> if (f x) then (x:) else id) [] list
