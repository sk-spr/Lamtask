module Main where
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import qualified Tasks
import qualified Config

printTaskList :: IO ()
printTaskList = do
    tasks <- Config.getStoredTasks "./tasks.txt"
    print (fromMaybe [] tasks)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> 
            printTaskList
        ["add-todo", desc] ->do
            storedTasks <- Config.getStoredTasks "./tasks.txt"
            Config.saveTasks "./tasks.txt" (Tasks.Todo desc : (fromMaybe [] storedTasks))