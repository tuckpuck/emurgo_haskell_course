module Main where 

-- IO is the way to deal with outside data or user submitted data in the pure functional Haskell world
-- IO is actually a Monad
-- With the IO pattern, you use 'do' notation
-- Inside Main, it is more like imperative programming that executes sequentially 
-- Main can include actions. All functions that don't need actions should be defined above
-- putStrLn is an IO action, not a function. All the IO types are special and considered actions when in IO

addUnder :: String -> String
addUnder str = "_" ++ str ++ "_"

main = do
  putStrLn "What is your name?"
  x <- getLine
  putStrLn ("Hello " ++ addUnder x)

