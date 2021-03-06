module Lesson21
  ( answerFib
  , helloMaybePerson
  , mainComparePizzas
  , mainDie
  , mainHelloPerson
  , maybeMain
  )
where

import qualified Data.Map                      as Map
import qualified Lesson08                      as L08
import qualified System.Random                 as R

-- Lesson 21.1
minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

mainDie :: IO ()
mainDie = do
  dieRoll <- R.randomRIO(minDie, maxDie)
  putStrLn (show dieRoll)

-- Lesson 21.2
helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

mainHelloPerson :: IO ()
mainHelloPerson = do
  putStrLn "Hello What's your name?"
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

-- Lesson 21.3
areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2) ^ 2

type Pizza = (Double, Double)

costPerCentimeter :: Pizza -> Double
costPerCentimeter (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 =
  if costPerCentimeter p1 < costPerCentimeter p2 then p1 else p2

describePizza :: Pizza -> String
describePizza (size, cost) =
  "The "
    ++ show size
    ++ " pizza is cheaper at "
    ++ show (costPerCentimeter (size, cost))
    ++ " per square centimeters"

mainComparePizzas :: IO ()
mainComparePizzas = do
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
  let betterPizza = comparePizzas pizza1 pizza2
  putStrLn (describePizza betterPizza)

costData :: Map.Map Int Double
costData = Map.fromList [(1, 1000.0), (2, 2000.0)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1, 18.0), (2, 26.0)]

maybeMain :: Maybe String
maybeMain = do
  size1 <- Map.lookup 1 sizeData
  cost1 <- Map.lookup 1 costData
  size2 <- Map.lookup 2 sizeData
  cost2 <- Map.lookup 2 costData
  let pizza1      = (size1, cost1)
  let pizza2      = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)

-- Q21-1
nameData :: Map.Map Int String
nameData = Map.fromList [(1, "太郎"), (2, "花子")]

helloMaybePerson :: Maybe String
helloMaybePerson = do
  name <- Map.lookup 1 nameData
  let statement = helloPerson name
  return statement

-- Q21-2
answerFib :: IO ()
answerFib = do
  putStrLn "何番目のフィボナッチ数が必要ですか?"
  n <- getLine
  let nthFib = L08.fib' (read n)
  putStrLn (n ++ "番目のフィボナッチ数は" ++ show nthFib)
