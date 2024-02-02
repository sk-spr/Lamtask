module Main where
import System.Environment (getArgs)
import System.Directory (renameFile, removeFile, doesFileExist)
import Data.Maybe (fromMaybe)
import  qualified Data.UnixTime (UnixTime(..), getUnixTime)
import qualified Tasks
import qualified Config
import Foreign.C (CTime(..))
import Text.Read (readMaybe)

-- from https://stackoverflow.com/questions/16191824/index-of-element-in-list-in-haskell
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [1..]

indexize :: [a] -> [(a, Int)]
indexize l = zip l [1..]

printTaskList :: Maybe [Tasks.Task] -> IO ()
printTaskList tasks = do
    (Data.UnixTime.UnixTime (CTime currentUnixSeconds) _) <- Data.UnixTime.getUnixTime
    case tasks of
        Just t -> do
            let
                indexed = indexize t
                events = filter (\(task, _) -> case task of
                        Tasks.Event _ (Tasks.UnixTimeStamp endSecs) _ -> endSecs > fromIntegral currentUnixSeconds
                        _ -> False
                    ) indexed
                todos = filter (\(task, _) -> case task of
                    Tasks.Todo _ -> True
                    Tasks.TimedTodo {} -> True
                    _ -> False) indexed
            case (events, todos) of
                ([],[]) -> putStrLn "Nothing coming up and no current Todos!"
                (events, todos) -> do
                    putStrLn "Events coming up: "
                    putStrLn $ mconcat $ map Tasks.prettyPrint events
                    putStrLn "Todos: "
                    putStrLn $ mconcat $ map Tasks.prettyPrint todos
        Nothing ->
            putStrLn "No tasks, yay!"

timeStampOrZero :: String -> Tasks.UnixTimeStamp
timeStampOrZero str = case Tasks.getTimeStamp str of
    Right stamp -> stamp
    _ -> Tasks.UnixTimeStamp 0

printHelp :: IO ()
printHelp = do
    putStrLn "Usage:"
    putStrLn "Lamtask -> print current tasks"
    putStrLn "Lamtask add-todo description -> create a simple todo with description"
    putStrLn "Lamtask add-timed hh:mm/[d]d.[m]m.[yyyy] description -> create a timed todo with due date and description"
    putStrLn "Lamtask add-event hh:mm/[d]d.[m]m.[yyyy] hh:mm/[d]d.[m]m.[yyyy] description-> create a calendar-style event from the first time to the second time with description"
    putStrLn "Lamtask show-all -> print all tasks, current or not"
    putStrLn "Lamtask remove n -> remove task number n"
    putStrLn "Lamtask remove all -> purge every task"

main :: IO ()
main = do
    args <- getArgs
    storedTasks <- Config.getStoredTasks "./tasks.txt"
    let
        switcharoo =
            do
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
        saveEvent event =
            do
                putStrLn ("Adding " <> Tasks.prettyPrint (event, 0))
                Config.saveTasks "./tasks.txt.new" (event : fromMaybe [] storedTasks)
                switcharoo
    case args of
            [] ->
                printTaskList storedTasks
            ["add-todo", desc] ->do
                saveEvent (Tasks.Todo desc)
            ["add-event", start, end, desc] ->
                let
                    startTime = timeStampOrZero start
                    endTime = timeStampOrZero end
                    event = Tasks.Event startTime endTime desc
                in
                    saveEvent event
            ["add-timed", due, desc] -> do
                saveEvent (Tasks.TimedTodo (timeStampOrZero due) desc)
            ["show-all"] ->
                case storedTasks of
                    Just t -> do
                        putStrLn $ mconcat $ map Tasks.prettyPrint $ indexize t
                    Nothing ->
                        putStrLn "No stored tasks."
            ["remove", n] ->
                let
                    num = readMaybe n :: Maybe Int
                in
                    case num of
                        Just i ->
                                let
                                    indexed = indexize $ fromMaybe [] storedTasks
                                in do
                                    putStrLn ("Removing entry " <> show i <> "...")
                                    Config.saveTasks "./tasks.txt.new" $ map fst $ filter (\(_, idx) -> idx /= i) indexed
                                    switcharoo
                        Nothing ->
                            if n == "all" then do
                                putStrLn "Removing task list entirely."
                                removeFile "./tasks.txt"
                            else
                                putStrLn "Could not parse the number you entered, make sure it is a valid index with \"Lamtask show-all\""
            _ -> printHelp
