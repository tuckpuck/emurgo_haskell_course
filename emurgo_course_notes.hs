module EmurgoCourseNotes where
-- import Emurgo_Haskell_Course (sum''')
import Data.List
import Data.Char
import Data.Monoid
import Data.Sequence (Seq(Empty))




doubleSmallNumber :: Int -> Int
doubleSmallNumber x = if x < 100 then x*2 else x

name = "Hello my name is Tucker"

mapString :: Char -> [Char] -> [Bool]
mapString c x = (filter (==True) (map (>=c) x)) ++ filter (==False) (map (>=c) x)

helloWorld = "hello" ++ "world"
listWorld = ["hello", "world"] ++ ["g","m"]
consString = 'h':"ello"
consList = "hello " : ["world"]



getFirstLetter :: String -> Char
getFirstLetter s = s !! 0


lists = [["a", "b"], ["c","f"]]
lists2 = lists ++ [["m", "h"]]

-- Lists are compared in lexicographical order. First the first one is compared. Only if they are equal then the second item is compared. That's why this evaluates to True.
compareLists = [3,2,1] > [2,10,100]

getTail = tail [1,2,3,4,5] --[2,3,4,5]
getHead = head [1,2,3,4,5] --1
getLast = last [1,2,3,4,5] --5
getAllButLast = init [1,2,3,4,5] --[1,2,3,4]
takeSomeFromList = take 3 [1,2,3,4,5,6]--[1,2,3]
dropFromStartOfList = drop 3 [1,2,3,4,5,6] --[4,5,6]

lengthOfList :: [a] -> Int
lengthOfList lst = length lst

checkIfListEmpty :: [a] -> Bool
checkIfListEmpty lst = null lst

reverseString :: String -> String
reverseString str = reverse str

getMax :: [Int] -> Int
getMax lst = maximum lst

getMin :: [Int] -> Int
getMin lst = minimum lst

checkIfIsInList :: Eq a => a -> [a] -> Bool
checkIfIsInList it lst = it `elem` lst


numRange = [1..100000]
charRange = ['a'..'x']
charRangeCap = ['A'..'X']
descendingList = [100,99..0]
reverseAlphabet1 = ['z', 'y'..'a']

reverseAlphabet2 = reverse ['a'..'z']

tenFives = take 10 (repeat 5)
tenOneTwoThrees = take 10 (cycle [1,2,3])

-- listOfLists  will throw an error because pairs and triples are different types, and lists require homogeneous types
-- listOfLists = [(1,3,2), (5,6,3), (2,9)]
tupleOfLists = ([1,2], [6,5,4], [2,4])
tupleOfTuples = ((1,2), (3,5), (3,8,6))


-- Fst and snd work on tuple pairs
getFirst = fst (3,9)
getSecond = snd (3,9)

-- Zip takes 2 lists and zips them into tuple pairs
zipThis = zip [1,2,3,4,5] [6,7,8,9,10]

-- You can use an infinite list for one zip param because haskell will stop at the length of the shorter list
zipThat = zip [1..] ["one", "two", "three", "four"]


-- Type declarations introduce a synonym for existing data types.
-- String was made this way 
-- type String = [Char]

type Point = (Float, Float)
combineVectors :: Point -> Point -> Point
combineVectors (w,x) (y,z) = (w+y, x+z)


type Name = String
type Phone = Int
type User = [(Name, Phone)]


-- Data Declarations create new types from existing types
-- "Define a type Color which can have the value of Red, Blue, and Green"
data Color = Red | Blue | Green
-- Color is data constructor. Red, Blue, Green are value constructors. They are values of 'Color'
grassColor = Green
-- :t grassColor 
-- grassColor :: Color

isRed :: Color -> Bool
isRed Red = True
isRed _ = False

-- Data constructors can also take arguments
type FullName = String
type Author = String
type Year = Int
-- Book combines Name Author Year as arguments
data Book = Book FullName Author Year deriving Show
hobbit = Book "the hobbit" "JRR Tolkien" 1937
atomicHabits = Book "atomic habits" "James Clear" 2018


capitalizeTitleAndAuthor :: Book -> Book
capitalizeTitleAndAuthor (Book na au ye) = Book (upperFirst na) (upperFirst au) ye

upperFirst :: [Char] -> [Char]
upperFirst = concat
           . map (\(c:cs) -> toUpper c : cs)
           . groupBy (\a b -> isSpace a == isSpace b)

-- type Height = Float
-- type Width = Float
-- type Radius = Float
-- data Shape = Rectangle Height Width | Circle Radius deriving Show

-- This could also be written like this, in the 'record' syntax
data Shape = Rectangle { height :: Float, width :: Float} | Circle { radius :: Float} deriving Show
-- Record syntax provides getters and setters to modify values of a data type
rect = Rectangle 30 20
rect2 = rect { width = 30 }
cir = Circle 3.2

calculateArea :: Shape -> Float
calculateArea (Rectangle h w) = h * w
calculateArea (Circle r) = pi * r**2

type Wheels = Int
type PaintColor = String
type Model = String
data Transport = Car Wheels PaintColor Model | Bike Wheels PaintColor Model deriving Show

suburu = Car 4 "Gray" "Forrester"
trek = Bike 2 "Red" "Trek"


isBikeOrCar :: Transport -> String
isBikeOrCar (Bike _ _ _) = "Bike"
isBikeOrCar (Car _ _ _) = "Car"



-- newtype is used when you have strictly one value constructor which takes strictly one argument
-- Box is the data constructor and value constructor
newtype Box a = Box { unBox :: a } deriving Show

myBox = Box 23
myBoxInt = unBox myBox





returnFloat :: Float -> Float -> Float
returnFloat x y = x * y

-- Polymorphic types
-- Accept a value of any type

-- Maybe is a polymorphic data constructor
-- data Maybe a = Nothing | Just a
-- Nothing and Just are value constructors
myVal = Just "World"
-- Just takes a value of type a and returns a value of Maybe a


-- List 
-- List is a polymorphic data constructor, with Empty and Cons as value constructors
-- data List a = Empty | Cons a (List a)



-- Instance declarations
-- Used to supply custom instructions for functions


-- Typeclass declarations
-- Typeclasses can be defined using the `type` keyword



-- Monad
-- >>= is the bind operator
-- >> Variant bind that discards previous computations
-- Maybe and IO are both monads
-- Lists, either, and pairs are also monads


-- Combine operations using Monoid
isForbidden :: Char -> Bool
isForbidden = getAny . foldMap (Any .) predicates
    where predicates = [isLower, isDigit]

-- Haskell compared to JS
-- No loops
-- No if 
-- Functions is a single return
-- No side effects (we can do this by separating pure functional and dirty IO)
-- No assignments within variables
-- No arrays
-- Functions can have only 0 or 1 arguments



fizzbuzz n
    | n `mod` 15 == 0 = "fizzbuzz"
    | n `mod` 5 == 0 = "fizz"
    | n `mod` 3 == 0 = "buzz"
    | otherwise = show n


-- Haskell is a pure functional programming language
-- No assignment statements
-- No variables 
-- Once given a value, never chnge
-- No side effects at all

-- Strive for modularity
-- Output will always be the same given an input
-- No concept of time
-- Reason about code with ease
-- Just follow types

-- Function without parameters is called a definition
-- ex. helloWorld = "Hello World"

-- Functions must start with a small letter

-- All functions with multiple arguments are actually currying

answerToEverything = 42
complexCalc :: Integer -> Integer -> Integer -> Integer
complexCalc x y z = x * y * z * answerToEverything
-- Due to currying, what this really means is 
-- complexCalc :: Integer -> (Integer -> (Integer -> Integer))
-- complexCalc 10 20 30 is really ((complexCalc 10) 20) 30


-- Haskell will infer types based on requirements of functions
london u = "London " ++ u

isA c = c == "a"

-- Char, Integers, Bool, Int, Float, Doubles, Tuples, Lists
doubleListOfNum :: [Int] -> [Int]
doubleListOfNum lst = map (*2) lst


-- Polymorphic types 
-- length :: [a] -> Int
-- 'a' is a type variable, which means length can be applied to list of any type


-- Lists
-- Can take an element from a list using !!
-- There is a 'null' method to say whether a list is empty
-- Head, tail, init, last are methods to get parts of a list
-- Drop drops the first n values and takes the rest
tryDrop = drop 3 [1,3,5,7]
-- Take takes the first n values
tryTake = take 3 [1,3,5,7]

binary = take 100 (cycle [0,1,1,0,0,1])
repeating = take 50 (repeat 4)

-- List comprehensions
findPowersToN :: Num a => Int -> [a]
findPowersToN n = take n [2 ^ x | x <- [1..]]

-- Create tuples with list comprehensions
createTuples = [(x,y) | x <- [1..5], y <- [1..5]]
-- An alternative way with fewer iterations
createTuples' = [(x,y) | x <- [1..5], y  <- [x..3]]

findEvens = [x | x <- [1..100], x `mod` 2 == 0]

-- Zip can be run using infinite lists
newTuple = zip [1,2,3,4,5,6] (cycle [0,1,2])
-- zipWith takes another argument and runs it on the pairs
newZipWith = zipWith (+) [1..100] [1..10]


-- Type classes are  a set that holds types. They define methods that the types can use. 
-- Read is used to convert to an integer
tryRead = read "5" + 10
-- If it doesn't have context, we can say what type it should go to 
tryRead' = read "5" :: Integer



-- Class constraint 
add :: Num a => a -> a -> a
add a b = a + b

-- Pattern matching
findLength :: Num a => [a] -> a
findLength [] = 0
findLength (x:xs) = 1 + findLength xs

second :: (a,b,c) -> b
second (_,y,_) = y


-- Data declarations and deriving existing type classes
data OS = Windows | OSX | Linux | Unix deriving (Eq, Ord, Show, Read)
-- Create a new type
class Bootable a where 
    boot :: a -> [Char]

instance Bootable OS where
    boot Unix = "Unix"
    boot Linux = "Linux"
    boot OSX = "OSX"
    boot Windows = "Windows"


-- Monoid is a typeclass for types that have a natural operation for combining values
-- Identity of a monoid is the value which satisfies the following i @ x = x, and x @ i = x for all possible values of x

-- Identities:
-- (+) is 0
-- (*) is 1
-- (&&) is True
-- (||) is False
-- (++) is []

-- <> is a synonym for mappend
-- mempty is the Identity of mappend


-- Modoids should follow a pattern that mempty is the Identity in all cases. 
-- ex.
-- "world" ++ []
-- [] ++ "world"
-- ("abc" ++ "def") ++ "ghi"
-- "abc" ++ ("def" ++ "ghi")

-- Int or FLoat can be Monoid in two ways.
-- (+) and 0, and (*) and 1

-- To get a type like Int to be a Monoid, you have to wrap it.
-- newtype Sum = Sum {getSum :: a}

-- instance Num a => Monoid (Sum a) where
--     mempty = Sum 0
--     Sum x <> Sum y = Sum (x + y)    

-- Monads capture the idea of a type which contains an identity with respect to an operator. 
-- Monoids implement mempty which represents an identity element and mappend for the operator

-- A monoid has elements
-- It is a binary operation
-- The operation is associative
-- There is a neutral element

-- Extends a binary operation of 2 inputs to allow many inputs. Encodes the way that operations can happen on many mathematical objects. 


-- Functor
-- A functor is a typeclass

data Maybe2 a = Just2 a | Nothing2 deriving Show

instance Functor Maybe2 where
    fmap func (Just2 a) = Just2 (func a)
    fmap func Nothing2 = Nothing2

-- Functor is called like this: (+3) <$> (Just2 4)
-- Identical to saying fmap (+3)(Just2 4)




data Tree a = Tip a | Branch (Tree a) (Tree a) deriving Show

myTree = Branch (Tip 4) (Branch (Tip 5)(Tip 6))

instance Functor Tree where
    fmap func (Tip a) = Tip (func a)
    fmap func (Branch left right) = Branch (fmap func left) (fmap func right)
-- Now, you can run functions on all items like this: fmap (+3) myTree



maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just (f x)


map1 = maybeMap (+100) (Just 30)
map2 = maybeMap (^4) (Just 100)


-- listMap :: (a -> b) -> List a -> List b
-- listMap _ Empty = Empty
-- listMap Cons (x xs) = Cons (f x) (listMap f xs)


data MakeList a = Item a | Empty deriving Show

myList = Item 1
myList2 = Item "hello"
myList3 = EmurgoCourseNotes.Empty

-- When making a function to do something similar into multiple elements, we can use the functor typeclass. 

