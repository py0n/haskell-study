module Main where

import           Lib
import qualified System.Environment            as E
import qualified Lesson21                      as L21

main :: IO ()
main = do
  args <- E.getArgs
  case head args of
    "mainHelloPerson" -> L21.mainHelloPerson
    _                 -> someFunc
