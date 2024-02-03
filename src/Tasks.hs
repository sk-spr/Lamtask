{-# LANGUAGE InstanceSigs #-}
module Tasks where

import Data.Maybe (maybeToList, fromMaybe)
import Text.Read (readMaybe)
import Control.Exception ( SomeException(..))

newtype DSecs = DSecs Int deriving Show
data DDate = DDate Int Int Int deriving Show

data Date = Date DSecs DDate deriving Show
newtype UnixTimeStamp = UnixTimeStamp Int deriving Show

instance Eq UnixTimeStamp where
    (==) :: UnixTimeStamp -> UnixTimeStamp -> Bool
    (==) (UnixTimeStamp a) (UnixTimeStamp b) = a == b

instance Ord UnixTimeStamp where
    (<=) :: UnixTimeStamp -> UnixTimeStamp -> Bool
    (<=) (UnixTimeStamp a) (UnixTimeStamp b) = a <= b

data Task =
    Event UnixTimeStamp UnixTimeStamp String
    | Todo String
    | TimedTodo UnixTimeStamp String
    deriving Show

split :: Eq a => [a] -> a -> [[a]]
split  = splitInner Nothing
splitInner :: Eq a => Maybe [a] -> [a] -> a -> [[a]]
splitInner ctx s delim =
    case s of
        [] ->
            case ctx of
                Just context ->
                    [context]
                Nothing ->
                    []
        c : cs ->
            case ctx of
                Just context ->
                    if c == delim then
                        context : splitInner Nothing cs delim
                    else
                        splitInner (Just (context <> [c])) cs delim
                Nothing ->
                    splitInner (Just [c]) cs delim

getTimeStamp :: String -> Int -> Either String UnixTimeStamp
getTimeStamp dateTimeStr now=
    let
        parts = split dateTimeStr '/'
    in
        case parts of
            [time, date] ->
                case (parseTime time, parseDate date now) of
                    (Right t, Right d) ->
                        Right $ georgianDateToUnix (Date t d)
                    (Left _, Right d) ->
                        Right $ georgianDateToUnix (Date (DSecs 0) d)
                    _ -> Left "Could not parse time."
            _ -> Left "Could not read time format."


parseTime :: String -> Either String DSecs
parseTime s =
    let
        parts = split s ':'
    in
    case parts of
        [hours, minutes]->
            let
                h = readMaybe hours :: Maybe Int
                m = readMaybe minutes :: Maybe Int
            in
                case (h,m) of
                    (Just hrs, Just mins) -> Right (DSecs (60 * (hrs * 60 + mins)))
                    _ -> Left "Unparseable time value!"
        _ ->
            Left "Unparseable time, should be in the format \"[h]h:[m]m\""

isLeapYear :: Int -> Bool
isLeapYear y = ((mod y 4 == 0) && (mod y 100 /= 0)) || mod y 400 == 0

daysOfMonth :: Int -> Int -> Int
daysOfMonth y m =
    case m of
        1 -> 31
        2 -> if isLeapYear y then 29 else 28
        3 -> 31
        4 -> 30
        5 -> 31
        6 -> 30
        7 -> 31
        8 -> 31
        9 -> 30
        10 -> 31
        11 -> 30
        12 -> 31
        _ -> 0

genCompleteYear :: Int -> Int
genCompleteYear y = if isLeapYear y then 366 else 365

getDaysBefore :: Int -> Int
getDaysBefore y = sum $ map genCompleteYear [1970..y-1]

genIncompleteYear :: Int -> Int -> Int -> Int
genIncompleteYear days months y =
    days - 1 + sum (map (daysOfMonth y) [1..months-1])

getDaysSinceEpoch :: Int -> Int -> Int -> Int
getDaysSinceEpoch day month year =
    getDaysBefore year + genIncompleteYear day month year

georgianDateToUnix :: Date -> UnixTimeStamp
georgianDateToUnix (Date (DSecs seconds) (DDate days months years )) = UnixTimeStamp $
    seconds + getDaysSinceEpoch days months years * 24 * 60 * 60

-- you are now entering the HACKY ZONE

getYear :: Int -> Int -> Int
getYear current timestamp =
    let
        (UnixTimeStamp y) = georgianDateToUnix (Date (DSecs 0) (DDate 1 1 current))
    in
        if  y > timestamp
        then
            current - 1
        else
            getYear (current+1) timestamp

getMonth :: Int -> Int -> Int -> Int
getMonth current timestamp year=
    let
        (UnixTimeStamp m) = georgianDateToUnix (Date (DSecs 0) (DDate 1 current year))
    in
        if m > timestamp
            then
                current - 1
            else
                getMonth (current+1) timestamp year

getDay :: Int -> Int -> Int -> Int -> Int
getDay current timestamp year month=
    let
        (UnixTimeStamp m) = georgianDateToUnix (Date (DSecs 0) (DDate current month year))
    in
        if m > timestamp
            then
                current - 1
            else
                getDay (current+1) timestamp year month

unixToGeorgianDate :: UnixTimeStamp -> Date
unixToGeorgianDate (UnixTimeStamp secs) =
    let
        year = getYear 1970 secs
        month = getMonth 1 secs year
        day = getDay 1 secs year month
        (UnixTimeStamp dateSecs) = georgianDateToUnix (Date (DSecs 0) (DDate day month year))
        remainder = secs - dateSecs
    in
        Date (DSecs remainder) (DDate day month year)

-- thank you for visiting the HACKY ZONE, come again!

getCurrentYear :: Int -> Int
getCurrentYear = getYear 1970

parseDate :: String -> Int -> Either String DDate
parseDate s now=
    let
        dateFromMaybes d m y =
            let
                ds = readMaybe d :: Maybe Int
                ms = readMaybe m :: Maybe Int
                ys = readMaybe y :: Maybe Int
            in
                case (ds,ms,ys) of
                    (Just days, Just months, Just years) ->
                        Right (DDate days months years)
                    (Just days, Just months, Nothing) ->
                        Right (DDate days months (getCurrentYear now))
                    _ -> Left "Could not parse date."
        parts = split s '.'
    in
        case parts of
            [days, months, years] ->
                dateFromMaybes days months years
            [days, months] ->
                dateFromMaybes days months ""
            _ ->
                Left "Could not parse date format. Needs to be [d]d.[m]m[.yyyy]"


-- storage parsing

parseTasksFrom :: String -> [Task]
parseTasksFrom = parseTaskList . lines

parseTaskList :: [String] -> [Task]
parseTaskList txts =
    case txts of
        [] -> []
        currentLine : rest ->
            parseTask currentLine : parseTaskList rest

parseTask :: String -> Task
parseTask line=
    case split line '°' of
        [taskType, taskDetails] ->
            (case taskType of
                "Event" ->
                    parseTaskEvent
                "Todo" ->
                    parseTaskTodo
                "TimedTodo" ->
                    parseTaskTimedTodo) taskDetails
        _ -> Todo ("Fix the config, something is not working on the line \"" <> line <> "\"")

parseTaskEvent :: String -> Task
parseTaskEvent s =
    case split s '$' of
        [startTime, endTime, description] ->
            let
                start = UnixTimeStamp $ fromMaybe 0 (readMaybe startTime :: Maybe Int)
                end = UnixTimeStamp $ fromMaybe 0 (readMaybe endTime :: Maybe Int)
            in
                Event start end description
        _ -> Todo "Fix the config, something is not working."

parseTaskTodo :: String -> Task
parseTaskTodo = Todo

parseTaskTimedTodo :: String -> Task
parseTaskTimedTodo s =
    case split s '$' of
        [dueDate, description] ->
            let
                due = UnixTimeStamp $ fromMaybe 0 (readMaybe dueDate :: Maybe Int)
            in
                TimedTodo due description

serializeTask :: Task -> String
serializeTask t =
    case t of
        Event (UnixTimeStamp start) (UnixTimeStamp end) description ->
            "Event°" <> show start <> "$" <> show end <> "$" <> description <> "\n"
        Todo description ->
            "Todo°" <> description <> "\n"
        TimedTodo (UnixTimeStamp due) description ->
            "TimedTodo°" <> show due <> "$" <> description <> "\n"

prettyTime :: DSecs -> String
prettyTime (DSecs secs) =
    let
        hours = div secs 3600
        minutes = div (secs - 3600 * hours) 60
        hourStr = case show hours of
            [digit] -> ['0', digit]
            digits -> digits
        minStr = case show minutes of
            [digit] -> ['0', digit]
            digits -> digits
    in
        hourStr <> ":" <> minStr

prettyDate :: DDate -> String
prettyDate (DDate day month year) =
    show day <> "." <> show month <> "." <> show year

prettyPrint :: UnixTimeStamp -> (Task,  Int) -> String
prettyPrint currentTime (t, n)= "("<>show n<>") ->" <>
    case t of
        Event startStamp endStamp description ->
            let
                Date start startDate = unixToGeorgianDate startStamp
                Date end endDate = unixToGeorgianDate endStamp
            in
                prettyTime start <> " on " <> prettyDate startDate <> " to " <> prettyTime end <> " on " <> prettyDate endDate <> " is an" <>(if  startStamp < currentTime then " ongoing" else "") <> " event called \"" <> description <> "\".\n"
        Todo description ->
            "To do: " <> description <> "\n"
        TimedTodo dueStamp description ->
            let
                Date dueTime dueDate = unixToGeorgianDate dueStamp
            in
               (if dueStamp < currentTime then "Overdue " else "") <>"To do: " <> description <> " due " <> prettyTime dueTime <> " on " <> prettyDate dueDate <> "\n"