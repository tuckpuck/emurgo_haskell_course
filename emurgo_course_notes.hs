module EmurgoCourseNotes where
-- import Emurgo_Haskell_Course (sum''')
import Data.List
import Data.Char
import Data.Monoid
import Data.Sequence (Seq(Empty), viewl)
import System.Console.Haskeline (mapInputT)
import Emurgo_Haskell_Course (x, y)




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


factorial :: Integer -> Integer
factorial 0 = 0
factorial 1 = 1
factorial x = x * (factorial (x - 1))


-- Haskell is built of functions, and we compose them together to get the functionality of the program. 


addInt :: (Int,Int) -> Int
addInt (a,b) = a + b




testMap = map (+1) [1..10]

map'' f xs = [f n | n <- xs]
testMap' = map'' (+1) [1..10]



listCompEx = [x + 1 | x <- [1..10], x `mod` 2 == 0]

filter' p xs = [n | n <- xs , p n]
testFilter' = filter' (>100) [1..1000]
testFilter'2 = filter' even [1..1000]



-- Program that tests character frequency in a string
-- run :: String -> [(Char, Int)]
-- run 

toLowerCaseLetters :: String -> String
toLowerCaseLetters = map toLower

removeSpaces :: String -> String
removeSpaces = filter (\x -> x /= ' ')

getFrequency = map (\x -> (head x, length x))

normalize = getFrequency (group (sort (toLowerCaseLetters (removeSpaces ['B', 'a', ' ', 'b']))))




-- Types
-- Type Synonyms
type ID = Int
type DOB = (Int, Int, Int)

mapIDs :: ID -> ID
mapIDs = (*2)


-- Build custom data types
-- data List = EmptyList | Cons Char (List)


-- Parameterized custom data types
data List a = EmptyList | Cons a (List a)

-- Convert a custom List type to String
toHList :: List a -> [a]
toHList EmptyList = []
toHList (Cons x xs) = x : toHList xs

map' :: (a -> a) -> [a] -> [a]
map' f [] = []
map' f (x:xs) = f x : map' f xs


-- This is a way to implement error handling
data Error a = Error | Ok a deriving Show

safeDivide :: Error Int -> Error Int -> Error Int 
safeDivide Error _ = Error
safeDivide _ Error = Error
safeDivide (Ok a) (Ok 0) = Error
safeDivide (Ok 0) (Ok b) = Error
safeDivide (Ok a) (Ok b) = Ok (a `div` b)



data Person = Person String Int (Int, Int, Int)

person1 = Person "James" 23 (22, 07, 1995)

getDob :: Person -> (Int, Int, Int)
getDob (Person _ _ dob) = dob


-- Record Syntax
data Human = Human { firstName :: String,
                           age :: Int, 
                           dob :: (Int, Int, Int) } deriving Show



-- Type classes
class Show' a where
    show' :: a -> String

instance Show' a => Show' (List a) where
    show' (EmptyList) = "Empty"
    show' (Cons a xs) = "Cons " ++ show' a ++ show' xs


data Number = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten deriving (Show, Ord, Eq)

numList = [Three, Nine, Ten, Four, Three, Two]
sortedList = sort numList


data Q = Q Integer Integer

instance Show Q where
    show (Q n d) = concat [show n, "/", show d]

instance Eq Q where
    r1 == r2 = (n1 == n2 && d1 == d2)
        where (Q n1 d1) = simpQ r1
              (Q n2 d2) = simpQ r2

addQ :: Q -> Q -> Q
addQ (Q a1 b1) (Q a2 b2) = Q (a1 + a2) (b1 + b2)

instance Num Q where 
    (+) = addQ
    negate (Q n d) = Q (-n) d
    (*) (Q n1 d1) (Q n2 d2) = simpQ (Q (n1 * n2) (d1 * d2))
    abs (Q n d) = Q (abs n) (abs d)
    signum (Q n d) = Q (signum n * signum d) 1
    fromInteger n = Q n 1


simpQ :: Q -> Q
simpQ (Q n d) = Q (n `div` c) (d `div` c)
    where c = gcd n d

-- Foldable is a type class that describes structures of values that can be summarized into a single value. 
-- To use foldable you have to define FoldMap, which translates a structure into a structure of monoidal values by applying mapply to each. 



sumListOfDigits :: [Int] -> Int
sumListOfDigits = foldr (+) 0

factorial' :: [Int] -> Int
factorial' = foldr (*) 1

fold :: (a -> b -> b) -> b -> [a] -> b
fold cons empty [] = empty
fold cons empty (x:xs) = x `cons` (fold cons empty xs)

-- Traversable is a type class that rebuilds the tree with additional applicative structure and having applicative effects along the way. It is an enhanced functor. 

-- State and Reader are additional type classes that are used to iteratively compose functions together. 



data Temperature = Celsius Float | Fahrenheit Float 


instance Show Temperature where
    show (Celsius x) = "The temperature is " ++ show x ++ " in Celsius"
    show (Fahrenheit n) = "The temperature is " ++ show n ++ " in Fahrenheit"

instance Eq Temperature where
    (==) (Celsius x) (Celsius y) = x == y
    (==) (Fahrenheit x) (Fahrenheit y) = x == y
    (==) (Celsius x) (Fahrenheit y) = (1.8 * x + 32) == y 
    (==) (Fahrenheit x) (Celsius y) = (1.8 * y + 32) == x 

instance Num Temperature where
    (+) (Celsius x) (Celsius y) = Celsius (x+y)
    (+) (Fahrenheit x) (Fahrenheit y) = Fahrenheit (x+y)
    (+) (Celsius x) (Fahrenheit y) = Celsius (x + (y-32)/1.8)
    (+) (Fahrenheit x) (Celsius y) = Fahrenheit (x + y * 1.8 + 32)
    negate (Celsius x) = Celsius (negate x)
    negate (Fahrenheit x) = Fahrenheit (negate x)
    (*) (Celsius x) (Celsius y) = Celsius (x * y)
    (*) (Fahrenheit x) (Fahrenheit y) = Fahrenheit (x * y)
    (*) (Celsius x) (Fahrenheit y) = Celsius (x * ((y-32)/1.8))
    (*) (Fahrenheit x) (Celsius y) = Fahrenheit(x * (y*1.8 + 32))
    signum (Celsius x) = Celsius (signum x)
    signum (Fahrenheit x) = Fahrenheit (signum x)
    fromInteger (x) = Celsius (fromInteger x)
    abs (Celsius x) = Celsius (abs x)
    abs (Fahrenheit x) = Fahrenheit (abs x)


-- Newtype gives some performace advantages, and allow different type class definitions on our new types
newtype RevString = RevString String

instance Show RevString where
    show (RevString s) = reverse s




-- Monoids 
-- mappend is associative and mempty is its identity element.
-- Mappend is a binary operation. It can also be called infix using <>
stringMappend = mappend "Julie" "Samuels"
stringMappendM = mappend "Julie" mempty
listMappend = mappend [1,2,3,4] [2,4,5,3] 
listMappendM = mappend mempty [2,3,4,4]

-- Using mconcat
-- Mconcat takes a list of elements and runs foldr mappend on the items
stringConcat = mconcat ["Julie", "Moronuki"]
stringConcatM = mconcat ["Julie", mempty]
doubleListConcat = mconcat [[1, 2], [4, 5]]
doubleListConcatM = mconcat [[1,2], mempty]
listConcat = mconcat [[1,2,3], [4,5,6]]
listConcatM = mconcat [mempty, [4,5,6], [6,4,5,6]]



-- Functors
-- Functors are a type class. The type class has a function fmap. Fmap is map essentially. Fmap is the minimal amount that needs to be defined to implement Functor class. 
-- Order of arguments is important
mapIt = map (+1) [1..10]
mapIt2 = fmap (+1) [1..10]
mapIt3 = (+1) <$> [1..10]
mapIt4 = fmap (+1) (Just 1)


data Failure' a = Fail' | Ok' a deriving (Show)

instance Functor Failure' where
    fmap f (Ok' x) = (Ok' $ f x)
    fmap f (Fail') = Fail'

tryFail = fmap (+1) (Fail')
tryOk = fmap (+1) (Ok' 10)


-- In lambda funcations you first take the parameters, then the function body
-- Lambdas can be applied to lambdas
-- Lambdas take only one variable by default. But they can be nested. (\a.(\b.(\c.(d.(\e.(~~~~~~)))))




returnMe' x = map (\z->z) x

addSomethingTwo x = (\x->x+1) x


-- Integral numbers
---- Int - Bit-restricted integer datatype
---- Integer - Can grow or shrink to arbitrary integer size
-- Fractional
---- Float - Memory limited decimal storage
---- Double - 
---- Rational - Arbitrary precision, stores number as evolving fraction. 
---- Scientific - Arbitrary precision
-- Char
-- List
-- String
-- Tuples
-- IO

-- IO means running a program than involves it may contain side effects
-- Tuples contain an arbitrary (but constant) amount of members
-- Tuples can have menbers of varying types 
-- Tuple conaining Int and String different than one containing String and Int
-- All items in a list must be the same type
-- Strings are a list of characters
cypherText :: [Char] -> [Char]
cypherText x = map (\z -> succ z) x

-- A typeclass guarantees certain properties or functionality
-- Types have an instance of a typeclass

-- Common typeclasses:
-- Num (Negation, conversion from Integer type, and sign)
-- Bounded (min and max)
-- Ord (Ordered by comparison > < >= <=)
-- EQ (Describes types where equality is knowable == /=)


-- Types
-- There is no untyped data in Haskell
-- The type of any expression, value, or function can be explicitly stated
-- Types of variables and functions are inferred if not stated explicitly
-- Code behaves more predictably due to the focus on types

-- Data declarations allow the creation of new types. They allow the creation of one or more Data Constructors

-- Type classes are like interfaces. They describe how the data is used rather than what it is. 

-- If a type is of a type class, you say that type has an instance of a type class

-- Polymorphism - type signatures that can represent more than one type
-- Parametric polymorphism knows nothing:
-- a -> b -> a
-- Constrained polymorphism:
-- (Eq a) => a -> a -> Bool
-- Type system keeps things general until evaluation must happen
-- Type classes are ordered and can specify prerequisites
-- Ex. EQ > Ord > Enum
 

-- Anything that can be accomplished via iteration can be done using recursion.
-- This is why Haskell has only recursion, no looping

-- Failed computations result in bottom
-- Infinite recursion results in an exception (a way in which bottom is handled)
-- Default lists are a form of singly linked lists

addMeUp :: [Int] -> Int
addMeUp [] = 0
addMeUp [x] = x
addMeUp (x:xs) = x + addMeUp xs

-- Zip pairs items of two lists into tuples
-- Unzip takes a list of tuples, and gives a tuple of lists
-- Folding summarizes the contents of a list. It requires an opration to combine 2 arguments. Also needs an accumulator.
-- For operations that are associative (like + for example. they are the same result whether applied from left to right) then the result won't change whether you use foldl or foldr

-- We often define new data types to make expressing our program easier.
-- Product type
data PairOfThings a = Pair a a
-- Sum Type
data Bool' = True' | False'
-- Often, new data types are a mix of the two. 

-- Some types require a type-level argument
-- Kinds are types for types
-- Only things of the kind '*' can be instantiated.
-- The number 3 has the type Int, and type Int has the kind '*'
-- PairOfThings have a kind of '* -> *'
-- Types that require arguments are known as higher kinded types.

-- Data Maybe = Nothing | Just a
-- Has the kind '* -> *

-- Data Either a b = Left a | Right b
-- Has the kind '* -> * -> *'

-- Monoid is a kind of algebra
-- Monoids all follow the laws of associativity and identity. 
-- The Identity is the value that, when added to a function, will not effect the result
-- id x = x. We call this value mempty in Haskell
-- All monoids can be reasonably folded.  
addLists = [1,2,3] <> [4,5,6]
combineLists = mconcat [[1,2,3], [4,5,6,7,8]]

-- Functor 
-- Functor has a container type f that holds value of type a
-- Functor has an fmap function that takes a function and lifts it.  
addToJust = fmap (+10) (Just 5) :: Maybe Int



-- Applicatives
-- All Applicatives also have to be Functors
-- This is an example of an applicative in Algebra: [(+1), (*2)] <*> [2, 4] == [3,5,4,8]
-- Applicative functions are Pure, and this infix function <*> also known as app
-- Applicative can be used as an alternative to || 
-- Applicatives allow you to apply a function to more than one argument
-- Applicatives are closely related to functors
-- Pure is a very simple function that puts a value in a data structure
-- <*> takes a function and a value and applies the function to the value
-- Maybe Applicative
app1 = pure (^2) <*> Just 2
app2 = pure (+) <*> Just 3 <*> Just 4
app3 = pure (*) <*> Just 5 <*> Just 6
-- List Applicative
app4 = pure (^ 2) <*> [ 2, 3, 4 ]
app5 = pure (+) <*> [ 2, 3 ] <*> [ 4, 5 ]
app6 = pure (*) <*> [ 2, 3, 4 ] <*> [ 5, 6, 7 ]
-- IO Applicative
app7 = pure tail <*> getLine
app8 = pure (^ 2) <*> (getLine >>= \x -> return $ read x)
app9 = pure (^ 2) <*> (getLine >>= \x -> return $ read x)



data Warn a = Warn | Okay a deriving (Show)

instance Functor Warn where
    fmap f Warn = Warn
    fmap f (Okay a) = Okay $ f a

instance Applicative Warn where
    pure x = Okay x
    (Okay f) <*> (Okay x) = (Okay $ f x)
    (Warn) <*> _ = Warn
    _ <*> (Warn) = Warn

data HomoSapein = HomoSapein String Int Int deriving (Show)


-- This one will throw a warning because one element is Falsy
appWarn = HomoSapein <$> (Okay "Tucker") <*> Warn <*> (Okay 43)
-- This one will use the data provided
appApprove = HomoSapein <$> (Okay "Tucker") <*> (Okay 20) <*> (Okay 43)

-- >>= is a bind operator. It's a way to run functions and return an IO value
-- Using bind and return, you can turn any input into IO
getInteger :: IO Int
getInteger = getLine >>= \s -> return (read s)

add' :: IO ()
add' = 
    getInteger >>= \a ->
    getInteger >>= \b ->
    print (a + b)

-- Could be called like this:
-- main :: IO ()
-- main = add'

sayName :: IO ()
sayName = getLine >>= (\name -> putStrLn ("hello " ++ name ++ "!"))



main :: IO ()
main = 
    -- Get two inputs
    putStrLn "Enter the first name" >>= \_ -> 
    getLine >>= \s ->
    putStrLn "Enter the second name" >>= \_ -> 
    getLine >>= \t -> 
    putStrLn ("hello " ++ t ++ " and " ++ s ++ "!")


-- PutStrLn doesn't really have a return, it is used only for its effects
-- Callbacks in monadic functions use the symbol 'k'


data Expr = Val Int | Div Expr Expr

expr1 = Val 1
expr2 = Div (Val 6) (Val 2)
expr3 = Div (Val 6) (Div (Val 3) (Val 1))

-- Original way to write, without Monads
-- eval :: Expr -> Maybe Int
-- eval (Val n) = Just n
-- eval (Div x y) = case eval x of 
--     Nothing -> Nothing
--     Just n -> case eval y of
--         Nothing -> Nothing
--         Just m -> safeDiv (eval n) (eval m)

-- Another way to write using bind notation
-- eval :: Expr -> Maybe Int
-- eval (Val n) = return n 
-- eval (Div x y) = eval x >>= (\n -> 
--                  eval y >>= (\m -> 
--                  safeDiv m n))

-- Eval using do notation
eval :: Expr -> Maybe Int
eval (Val n) = return n
eval (Div x y) = do n <- eval                                                       sdx
                    m <- eval y 
                    safeDiv n m

    
-- m >>= f = case m of 
--     Nothing -> Nothing
--     Just x -> f x

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y = if x == 0 then Nothing else Just (div x y)

-- Maybe type constructor has two Monadic functions available
-- return takes any value and converts to a maybe value. Gives a bridge between pure and impure worlds. 
-- >>= allows sequencing. Give this something that can fail, and a function for if it succeeds. 

-- Same idea works for other effects (IO, mutable states, non-determinism). Gives a uniform framework for programming with effects.
-- Supports pure programming with effects.
-- Use of effects is implicit in types
-- Functions that work for any effect. 