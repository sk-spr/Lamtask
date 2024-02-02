module Main where
import System.Environment (getArgs)
import System.Directory (renameFile, removeFile, doesFileExist)
import Data.Maybe (fromMaybe)
import qualified Tasks
import qualified Config

-- from https://stackoverflow.com/questions/16191824/index-of-element-in-list-in-haskell
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [1..]

printTaskList :: Maybe [Tasks.Task] -> IO ()
printTaskList tasks =
    case tasks of
        Just t ->
            putStrLn $ mconcat $ mapInd Tasks.prettyPrint t
        Nothing ->
            putStrLn "No tasks, yay!"

main :: IO ()
main = do
    args <- getArgs
    storedTasks <- Config.getStoredTasks "./tasks.txt"
    let
        saveEvent event =
            do
                Config.saveTasks "./tasks.txt.new" (event : fromMaybe [] storedTasks)
                oldFileExists <- doesFileExist "./tasks.txt.old"
                if oldFileExists then
                    do
                        removeFile "./tasks.txt.old"
                    else
                        do
                            putStrLn "Backing up for the first time..."
                tasksExists <- doesFileExist "./tasks.txt"
                if tasksExists then
                    do
                        renameFile "./tasks.txt" "./tasks.txt.old"
                    else
                        do
                            putStrLn "Starting new tasks.txt..."
                renameFile "./tasks.txt.new" "./tasks.txt"
    case args of
            [] ->
                printTaskList storedTasks
            ["add-todo", desc] ->do
                saveEvent (Tasks.Todo desc)
            ["add-event", start, end, desc] ->
                let
                    timeStampOrZero str = case Tasks.getTimeStamp str of
                        Right stamp -> stamp
                        _ -> Tasks.UnixTimeStamp 0
                    startTime = timeStampOrZero start
                    endTime = timeStampOrZero end
                    event = Tasks.Event startTime endTime desc
                in
                    saveEvent event
