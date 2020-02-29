module Lib
  ( ABOType(A, B, AB, O)
  , BloodType(BloodType)
  , Name(Name, NameWithMiddle)
  , Patient(Patient)
  , RhType(Pos, Neg)
  , Sex(Male, Female)
  , concatAll
  , cycleSucc
  , fib
  , fib'
  , harmonic
  , inFirstHalf
  , isPalindrome
  , myDrop
  , myElem
  , myGCD
  , myLength
  , myProduct
  , myReverse
  , myReverse'
  , myTail
  , myTake
  , patientSummary
  , printDouble
  , remove
  , remove'
  , someFunc
  , subseq
  )
where

import           Data.Char                      ( isAlpha
                                                , toUpper
                                                )

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

-- Quick Check 9-1
remove :: (a -> Bool) -> [a] -> [a]
remove _ []       = []
remove f (x : xs) = if f x then remove f xs else x : remove f xs

remove' f xs = foldr (\x a -> if f x then a else x : a) [] xs

-- Quick Check 9-2
myProduct :: (Foldable t, Num b) => t b -> b
myProduct xs = foldr (*) 1 xs

-- Lesson 9-4
concatAll :: Foldable t => t [a] -> [a]
concatAll xss = foldr (++) [] xss

-- Q9-1
myElem z xs = length (filter (\x -> x == z) xs) > 0

-- Q9-2
isPalindrome :: String -> Bool
isPalindrome xs = ys == reverse ys where ys = map toUpper (filter isAlpha xs)

-- Q9-3
harmonic :: Fractional a => Int -> a
harmonic n = sum (take n (map (\x -> 1 / fromIntegral x) [1 ..]))

-- Quick Check 11-2
printDouble :: Int -> String
printDouble n = show (n * 2)

-- Q12-1
type FirstName = String
type MiddleName = String
type LastName = String
data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName
  deriving (Eq, Show)

data Sex = Male | Female deriving (Eq, Show)

data RhType = Pos | Neg deriving (Eq, Show)
data ABOType = A | B | AB | O deriving (Eq, Show)
data BloodType = BloodType ABOType RhType deriving (Eq, Show)

data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType } deriving (Eq, Show)

canDonateTo :: Patient -> Patient -> Bool
canDonateTo p q = g (bloodType p) (bloodType q)
 where
  g (BloodType _ Pos) (BloodType _ Neg) = False
  g (BloodType _ Neg) (BloodType _ Pos) = False
  g (BloodType O _  ) _                 = True
  g _                 (BloodType AB _)  = True
  g (BloodType A _)   (BloodType A  _)  = True
  g (BloodType B _)   (BloodType B  _)  = True
  g _                 _                 = False

-- Q12-2
patientSummary :: Patient -> String
patientSummary p =
  "**********\n"
    ++ "Patient Name: "
    ++ lastName (name p)
    ++ ", "
    ++ firstName (name p)
    ++ "\n"
    ++ "Sex: "
    ++ show (sex p)
    ++ "\n"
    ++ "Age: "
    ++ show (age p)
    ++ "\n"
    ++ "Height: "
    ++ show (height p)
    ++ " in.\n"
    ++ "Weight: "
    ++ show (weight p)
    ++ " lbs.\n"
    ++ "Blood Type: "
    ++ showBloodType (bloodType p)
    ++ "\n**********\n"
 where
  lastName (Name _ l            ) = l
  lastName (NameWithMiddle _ _ l) = l
  firstName (Name f _            ) = f
  firstName (NameWithMiddle f _ _) = f
  showABO (BloodType A  _) = "A"
  showABO (BloodType B  _) = "B"
  showABO (BloodType AB _) = "AB"
  showABO (BloodType O  _) = "O"
  showRh (BloodType _ Pos) = "+"
  showRh (BloodType _ Neg) = "-"
  showBloodType b = showABO b ++ showRh b

-- Q13-3
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n | n == maxBound = minBound
            | otherwise     = succ n
