-- Imperative performs mutation of state by assignment of variables
-- Functional emulates mutation of state by transformation of values by applying functions. 

-- Lambda expression is an anonymous function that uses special syntax
double :: [Int] -> [Int]
double = map addToSelf

addToSelf :: Int -> Int
addToSelf = (\x -> x + x)

-- Every lambda expression is a function
-- Every function is a lambda expression under the hood
-- Lambda expressions are mostly declared inline
-- Haskell infers the type of lambda functions, but we could also declare it
-- For example, (\x -> x + x) :: Int -> Int

multiply :: Integer -> Integer -> Integer
multiply = (\x y -> x * y)
doExp = (\x y z -> x^y^z)
concatWords = (\word1 word2 -> word1 ++ " " ++ word2) :: [Char] -> [Char] -> [Char]

-- Operator section is a syntactic shortcut for lambda functions
-- (@ y) is the same as (\x -> x @ y)
-- (+2) is the same as (\x -> x + 2)
-- (++ "world") is the same as (\x -> x ++ "world")
plus = (+2) 3
times = (3 *) 4
comp=(4>)5
words1 = ("hello" ++ ) ", world"
words2 = (++ "hello") ", world"
division = (/ 2) 7
-- (^2) is (\x -> x ^ 2) but (2^) is (\x -> 2 ^ x)
exp1 = (^2) 7
exp2 = (2^) 7


-- Conditionals. A construct which allows the program to decide between two or more alternatives. 
-- Write a function, fizzbuzz that returns "Fizz" if divisible by three but not by five, "Buzz" if not divisible by three but it is by five, or "fizzbuzz" if divisible by three or five, just n otherwise
-- if
fizzBuzz :: Int -> String
fizzBuzz n = if (n `mod` 3 == 0 && n `mod` 5 == 0) then "fizzbuzz" 
    else if (n `mod` 3 == 0) then "fizz"
    else if (n `mod` 5 == 0) then "buzz"
    else show n

-- case
fizzBuzz' :: Int -> String
fizzBuzz' n = case (n `mod` 3 == 0, n `mod` 5 == 0) of
    (True, True) -> "fizzbuzz"
    (True, False) -> "fizz"
    (False, True) -> "buzz"
    (False, False) -> show n 

-- guard 
fizzBuzz'' :: Int -> String
fizzBuzz'' n
    | (n `mod` 3 == 0 && n `mod` 5 == 0) = "fizzbuzz"
    | n `mod` 3 == 0 = "fizz"
    | n `mod` 5 == 0 = "buzz"
    | otherwise = show n

-- If can be assigned to variables
yyy = if (5 > 3) then "hello" else "goodbye"
-- If can be passed into functions
headyyy = head yyy 

-- Case can be assigned to variables
x = case (2 + 3) of 
    5 -> "Correct"
    _ -> "Wrong"
-- Case can be passed into functions
caseLen = length $ x

and' :: Bool -> Bool -> Bool
and' x y 
    | x == True && y == True = True
    | otherwise = False

-- Expressions vs statements
-- Expressions evaluate to values, whereas statements perform instructions
-- Everything in Haskell is an expression, nothing is a statment

-- Patterns 
-- Patterns allow us to destructure values and capture variables

-- Patterns can match tuples
reverseTuple :: (x, y) -> (y, x)
reverseTuple (x,y) = (y, x)

fst :: (a, b) -> a
fst (a, _) = a

snd :: (a,b) -> b
snd (_, b) = b

-- Patterns can match lists
isSame = [1,2,3,4] == 1 : (2 : (3 : (4 :[])))
-- Lists are constructed by the : operator, and are decomposed by head and tail functions

isSame2 = 2 : (3 : (4 : (5 : (6 : (7 : []))))) == [2,3,4,5,6,7]
checkHead = head (2 : (3 : (4 : (5 : (6 : (7 : []))))))
checkTail = tail (2 : (3 : (4 : (5 : (6 : (7 : []))))))

-- Common pattern to deconstruct lists
(y:ys) = [2,3,4,5]
(c:cs) = "world"
[m,n,o] = [10,20,30]

-- Can be used to get certain items out of a list
getHead :: [a] -> a
getHead (x:xs) = x

getTail :: [a] -> [a]
getTail (y:ys) = ys

getLast :: [a] -> Maybe a
getLast [] = Nothing
getLast [x] = Just x
getLast (x:xs) = Just (last xs)

-- Recursion
-- Recursion is when a function is defined in terms of itself
-- Haskell has no loops, only uses recursion
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial x = x * factorial (x - 1)



(xs) = [1,2]

add' = \x y -> x + y

(x', y' : ys', z') = (23, "world", 'x')


