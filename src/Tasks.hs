module Tasks where

import Data.Maybe (maybeToList, fromMaybe)
import Text.Read (readMaybe)
import Control.Exception ( SomeException(..))

newtype DSecs = DSecs Int deriving Show
data DDate = DDate Int Int Int deriving Show

data Date = Date DSecs DDate
newtype UnixTimeStamp = UnixTimeStamp Int deriving Show

data Task =
    Event UnixTimeStamp UnixTimeStamp String
    | Todo String
    | TimedTodo UnixTimeStamp String

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

getTimeStamp :: String -> Either String UnixTimeStamp
getTimeStamp dateTimeStr =
    let
        parts = split dateTimeStr '/'
    in
        case parts of
            [time, date] ->
                case (parseTime time, parseDate date) of
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

getDaysSinceEpoch :: Int -> Int -> Int -> Int
getDaysSinceEpoch day month year =
    let
        genCompleteYear y = if isLeapYear y then 366 else 365
        getDaysBefore y = sum $ map genCompleteYear [1970..y-1]
        genIncompleteYear days months y =
            days - 1 + sum (map (daysOfMonth y) [1..months-1])
    in
        getDaysBefore year + genIncompleteYear day month year

georgianDateToUnix :: Date -> UnixTimeStamp
georgianDateToUnix (Date (DSecs seconds) (DDate days months years )) = UnixTimeStamp $
    seconds + getDaysSinceEpoch days months years * 24 * 60 * 60

getCurrentYear :: () -> Int
getCurrentYear () = 2024 -- todo implement

parseDate :: String -> Either String DDate
parseDate s =
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
                        Right (DDate days months (getCurrentYear ()))
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