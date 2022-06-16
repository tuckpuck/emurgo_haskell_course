module IO where

main :: IO ()
main = 
    putStrLn "Hello world" 

readInt :: IO Int
readInt = read <$> getLine

-- IO is a functor
-- IO is also applicative

addInts :: IO Int
addInts = (+) <$> readInt <*> readInt


askName :: IO String
askName = putStrLn "Hello" *> putStr "What is your name? " *> getLine 


printLines :: [String] -> IO ()
printLines ls = traverse putStrLn ls *> pure ()