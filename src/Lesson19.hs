module Lesson19
  ( numOrZero
  )
where

-- Quick Check 19-2
numOrZero :: Maybe Int -> Int
numOrZero Nothing  = 0
numOrZero (Just n) = n
