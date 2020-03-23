module Lesson21
  ( helloPerson
  )
where

-- Lesson 21.2
createHello :: String -> String
createHello name = "Hello " ++ name ++ "!"

helloPerson :: IO ()
helloPerson = do
  putStrLn "Hello What's your name?"
  name <- getLine
  let statement = createHello name
  putStrLn statement
