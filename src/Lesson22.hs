module Lesson22
  ( mainShowArgs
  , mainShowReversedUserInput
  , mainShowUserInput
  )
where

import qualified System.Environment            as E

-- Lesson 22.1
mainShowArgs :: IO ()
mainShowArgs = do
  args <- E.getArgs
  mapM_ putStrLn args

-- Lesson 22.2
mainShowUserInput :: IO ()
mainShowUserInput = do
  userInput <- getContents
  mapM_ print userInput

-- Quick Check 22-3
mainShowReversedUserInput :: IO ()
mainShowReversedUserInput = do
  userInput <- getContents
  mapM_ print (reverse userInput)
