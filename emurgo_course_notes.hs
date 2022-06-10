module EmurgoCourseNotes where
import Emurgo_Haskell_Course (sum''')
import Data.List
import Data.Char




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

