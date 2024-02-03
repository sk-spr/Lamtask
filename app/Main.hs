module Main where
import System.Environment (getArgs)
import System.Directory (renameFile, removeFile, doesFileExist)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import Paths_Lamtask  (version)
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
                printOut :: [(Tasks.Task, Int)] -> IO ()
                printOut evs= putStrLn $ mconcat $ map (Tasks.prettyPrint  (Tasks.UnixTimeStamp (fromIntegral currentUnixSeconds))) evs
            case (events, todos) of
                ([],[]) -> putStrLn "Nothing coming up and no current Todos!"
                (events, todos) -> do
                    putStrLn "Events coming up: "
                    printOut events
                    putStrLn "Todos: "
                    printOut todos
        Nothing ->
            putStrLn "No tasks, yay!"

timeStampOrZero :: String -> Int -> Tasks.UnixTimeStamp
timeStampOrZero str now= case Tasks.getTimeStamp str now of
    Right stamp -> stamp
    _ -> Tasks.UnixTimeStamp 0

printHelp :: IO ()
printHelp = do
    putStrLn "Usage:"
    putStrLn "Lamtask f=taskfile -> print current tasks"
    putStrLn "Lamtask f=taskfile add-todo description -> create a simple todo with description"
    putStrLn "Lamtask f=taskfile add-timed hh:mm/[d]d.[m]m.[yyyy] description -> create a timed todo with due date and description"
    putStrLn "Lamtask f=taskfile add-event hh:mm/[d]d.[m]m.[yyyy] hh:mm/[d]d.[m]m.[yyyy] description-> create a calendar-style event from the first time to the second time with description"
    putStrLn "Lamtask f=taskfile show-all -> print all tasks, current or not"
    putStrLn "Lamtask f=taskfile remove n -> remove task number n"
    putStrLn "Lamtask f=taskfile remove all -> purge every task"

main :: IO ()
main = do
    args <- getArgs
    (Data.UnixTime.UnixTime (CTime currentUnixSeconds) _) <- Data.UnixTime.getUnixTime
    let
        switcharoo :: FilePath -> IO ()
        switcharoo baseFileName=
            do
                oldFileExists <- doesFileExist (baseFileName <>".old")
                if oldFileExists then
                    do
                        removeFile (baseFileName <> ".old")
                    else
                        do
                            putStrLn "Backing up for the first time..."
                tasksExists <- doesFileExist baseFileName
                if tasksExists then
                    do
                        renameFile baseFileName (baseFileName <> ".old")
                    else
                        do
                            putStrLn "Starting new tasks.txt..."
                renameFile (baseFileName<>".new") baseFileName
        saveEvent event fileName taskList =
            do
                putStrLn ("Adding " <> Tasks.prettyPrint (Tasks.UnixTimeStamp (fromIntegral currentUnixSeconds)) (event, 0))
                Config.saveTasks (fileName <> ".new") (event : fromMaybe [] taskList)
                switcharoo fileName
        now = fromIntegral currentUnixSeconds
    case args of
        ['f':'=':filename] ->do
            oldTasks <- Config.getStoredTasks filename
            printTaskList oldTasks
        ['f':'=':filename, "add-todo", desc] -> do
            oldTasks <- Config.getStoredTasks filename
            saveEvent (Tasks.Todo desc) filename oldTasks
        ['f':'=':filename, "add-event", start, end, desc] ->
            let
                startTime = timeStampOrZero start now
                endTime = timeStampOrZero end now
                event = Tasks.Event startTime endTime desc
            in do
                oldTasks <- Config.getStoredTasks filename
                saveEvent event filename oldTasks
        ['f':'=':filename, "add-timed", due, desc] -> do
            oldTasks <- Config.getStoredTasks filename
            saveEvent (Tasks.TimedTodo (timeStampOrZero due now) desc) filename oldTasks
        ['f':'=':filename, "show-all"] ->do
            oldTasks <- Config.getStoredTasks filename
            case oldTasks of
                Just t -> do
                    putStrLn $ mconcat $ map (Tasks.prettyPrint (Tasks.UnixTimeStamp (fromIntegral currentUnixSeconds))) $ indexize t
                Nothing ->
                    putStrLn "No stored tasks."
        ['f':'=':filename, "remove", n] ->
            let
                num = readMaybe n :: Maybe Int
            in
                case num of
                    Just i ->
                            let
                                indexed = indexize . fromMaybe []
                            in do
                                oldTasks <- Config.getStoredTasks filename
                                putStrLn ("Removing entry " <> show i <> "...")
                                Config.saveTasks (filename <> ".new") $ map fst $ filter (\(_, idx) -> idx /= i) (indexed oldTasks)
                                switcharoo filename
                    Nothing ->
                        if n == "all" then do
                            putStrLn "Removing task list entirely."
                            removeFile filename
                        else
                            putStrLn "Could not parse the number you entered, make sure it is a valid index with \"Lamtask show-all\""
        ["help"] -> do
            putStrLn ("Lamtask version " <> showVersion version)
            printHelp
        _ -> printHelp