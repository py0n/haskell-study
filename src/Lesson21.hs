module Lesson21
  ( comparePizza
  , helloPerson
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

-- Lesson 21.3
areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2) ^ 2

type Pizza = (Double, Double)

costPerCentimeter :: Pizza -> Double
costPerCentimeter (size, cost) = cost / areaGivenDiameter size

comparePizzaCost :: Pizza -> Pizza -> Pizza
comparePizzaCost p1 p2 =
  if costPerCentimeter p1 < costPerCentimeter p2 then p1 else p2

describePizza :: Pizza -> String
describePizza (size, cost) =
  "The "
    ++ show size
    ++ " pizza is cheaper at "
    ++ show (costPerCentimeter (size, cost))
    ++ " per square centimeters"

comparePizza :: IO ()
comparePizza = do
  putStrLn "What is the size of pizza 1"
  size1 <- getLine
  putStrLn "What is the cost of pizza !"
  cost1 <- getLine
  putStrLn "What is the size of pizza 2"
  size2 <- getLine
  putStrLn "What is the cost of pizza !"
  cost2 <- getLine
  let pizza1      = (read size1, read cost1)
  let pizza2      = (read size2, read cost2)
  let betterPizza = comparePizzaCost pizza1 pizza2
  putStrLn (describePizza betterPizza)
