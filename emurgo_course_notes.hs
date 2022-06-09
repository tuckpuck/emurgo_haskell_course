module EmurgoCourseNotes where
import Emurgo_Haskell_Course (sum''')


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
