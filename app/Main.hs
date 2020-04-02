module Main where

import           Lib
import qualified Lesson21                      as L21
import qualified Lesson22                      as L22
import qualified System.Environment            as E

main :: IO ()
main = do
  args <- E.getArgs
  case head args of
    "answerFib"                 -> L21.answerFib
    "mainComparePizzas"         -> L21.mainComparePizzas
    "mainDie"                   -> L21.mainDie
    "mainHelloPerson"           -> L21.mainHelloPerson
    "mainShowArgs"              -> L22.mainShowArgs
    "mainShowReversedUserInput" -> L22.mainShowReversedUserInput
    "mainShowSumSquare"         -> L22.mainShowSumSquare
    "mainShowUserInput"         -> L22.mainShowUserInput
    _                           -> someFunc
