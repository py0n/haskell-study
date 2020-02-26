module Lib
  ( inFirstHalf
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
