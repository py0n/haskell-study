module Lib
  ( inFirstHalf
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
