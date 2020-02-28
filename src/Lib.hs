module Lib
  ( fib
  , fib'
  , inFirstHalf
  , myDrop
  , myGCD
  , myLength
  , myReverse
  , myReverse'
  , myTail
  , myTake
  , someFunc
  , subseq
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Q6-2
subseq :: Int -> Int -> [a] -> [a]
subseq m n xs = take (n - m) $ drop m xs

-- Q6-3
inFirstHalf :: Eq a => a -> [a] -> Bool
inFirstHalf x xs = x `elem` take p xs where p = length xs `div` 2

-- Lesson7
myTake :: (Eq t, Num t) => t -> [a] -> [a]
myTake _ []       = []
myTake 0 _        = []
myTake m (x : xs) = x : myTake (m - 1) xs

-- Quick Check 7-3
myTail :: [a] -> [a]
myTail []       = []
myTail (_ : xs) = xs

-- Q7-2
myGCD :: Integral t => t -> t -> t
myGCD a b | b == 0    = a
          | otherwise = myGCD b (a `mod` b)

-- Lesson8
myDrop :: (Eq t, Num t) => t -> [a] -> [a]
myDrop _ []       = []
myDrop 0 xs       = xs
myDrop m (x : xs) = myDrop (m - 1) xs

-- Quick Check 8-1
myLength []       = 0
myLength (x : xs) = 1 + myLength xs

-- Q8-1
myReverse :: [a] -> [a]
myReverse xs = g [] xs
 where
  g ys []       = ys
  g ys (x : xs) = g (x : ys) xs

myReverse' :: Foldable t => t a -> [a]
myReverse' xs = foldl (\a x -> x : a) [] xs

-- Q8-2
fib :: (Eq a, Num a, Num p) => a -> p
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib' :: (Eq t1, Num t1, Num t2) => t1 -> t2
fib' n = fastFib 0 1 n
 where
  fastFib a _ 0 = a
  fastFib a b n = fastFib b (a + b) (n - 1)
