module Main where

import           Lib
import qualified System.Environment            as E
import qualified Lesson21                      as L21

main :: IO ()
main = do
  args <- E.getArgs
  case head args of
    "answerFib"         -> L21.answerFib
    "mainComparePizzas" -> L21.mainComparePizzas
    "mainDie"           -> L21.mainDie
    "mainHelloPerson"   -> L21.mainHelloPerson
    _                   -> someFunc
