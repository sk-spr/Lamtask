module Main where

import System.Environment (getArgs)
import qualified Tasks
main :: IO ()
main = do
    line <- getLine
    print $ Tasks.getTimeStamp line