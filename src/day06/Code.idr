module Main

import Data.List
import Data.Maybe
import Data.String
import System.File

parseFile : (path : String) -> IO (String, String)
parseFile path =
    let
        go : (String, String) -> File -> IO (Either FileError (String, String))
        go (times, distances) file = do
            False <- fEOF file | True => pure (Right (times, distances))
            Right line <- fGetLine file
                | Left err => pure (Left err)
            if isPrefixOf "Time:" line
                then go (snd $ break (isSpace) line, distances) file
                else if isPrefixOf "Distance:" line
                    then go (times, snd $ break (isSpace) line) file
                    else go (times, distances) file
    in do
        result <- withFile path Read pure (go ("", ""))
        case result of
            (Left err) => pure ("", "")
            (Right n) => pure n

wins : (Integer, Integer) -> Integer
wins (time, distance) = time - 2 * (threshold 0) +1
    where 
        threshold : Integer -> Integer
        threshold i = if (time - i) * i > distance then i else threshold (i + 1)

-- PART I

parseNumbers : String -> List Integer
parseNumbers str = reverse $
    foldl
        (\acc, elm =>
            case parseInteger elm of
                Just v => v :: acc
                Nothing => acc
        )
        []
        (words str)

partOne : String -> String -> Integer
partOne t d =
    let
        races := zip (parseNumbers t) (parseNumbers d)
    in
        foldl (\acc, elem => acc * wins elem) 1 races

-- PART II

parseCombinedNumber : String -> Integer
parseCombinedNumber str = fromMaybe 0 $ parseInteger $ pack $ filter (not . isSpace) $ unpack str 

partTwo : String -> String -> Integer
partTwo t d =
    wins (parseCombinedNumber t, parseCombinedNumber d)

-- Main

main : IO ()
main = do
    (t, d) <- parseFile "src/day06/input.txt"
    printLn $ "Part  I result: " ++ cast (partOne t d)
    printLn $ "Part II result: " ++ cast (partTwo t d)
