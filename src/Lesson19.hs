module Lesson19
  ( numOrZero
  , organCatalog
  , processRequest
  )
where

import           Data.Map                      as Map
import           Data.Maybe                     ( isNothing )

-- Lesson 19
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organCatalog :: Map Int Organ
organCatalog = Map.fromList
  [ (2 , Heart)
  , (7 , Heart)
  , (13, Brain)
  , (14, Spleen)
  , (21, Spleen)
  , (24, Kidney)
  ]

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Bag    o) = show o ++ " in a bag"
  show (Cooler o) = show o ++ " in a cooler"
  show (Vat    o) = show o ++ " in a vat"

data Location = Lab | Kitchen | Bathroom deriving (Show)

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer o     = Bag o

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat    a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag    a) = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process o = placeInLocation (organToContainer o)

report :: (Location, Container) -> String
report (l, c) = show c ++ " in the " ++ show l

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
  where organ = Map.lookup id catalog

processAndReport :: Maybe Organ -> String
processAndReport Nothing      = "error, id not found"
processAndReport (Just organ) = report (process organ)

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = Prelude.map getContents ids
  where getContents = \id -> Map.lookup id catalog

-- Quick Check 19-2
numOrZero :: Maybe Int -> Int
numOrZero Nothing  = 0
numOrZero (Just n) = n

-- Q19-1
emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers = length . Prelude.filter isNothing
