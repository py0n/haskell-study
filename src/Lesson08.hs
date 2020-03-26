module Lesson08
  ( fib
  , fib'
  , myDrop
  , myLength
  , myReverse
  , myReverse'
  )
where

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
