module Config where
import qualified Tasks
import System.Directory (doesFileExist)

type TaskList = [Tasks.Task]

getStoredTasks :: FilePath -> IO (Maybe TaskList)
getStoredTasks file = do
    exists <- doesFileExist file
    if exists then do
        contents <- readFile file
        pure $ Just $ Tasks.parseTasksFrom contents
    else
        pure Nothing

saveTasks :: FilePath -> TaskList -> IO ()
saveTasks file tasks =
    writeFile file (mconcat $ map Tasks.serializeTask tasks)