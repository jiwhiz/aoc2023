module Main

import Data.List
import Data.Maybe
import Data.String
import System.File

-- PART One

partOne : List Char -> Maybe Integer
partOne chars = do
    first <- find isDigit chars
    last <- find isDigit (reverse chars)
    parseInteger $ pack [first, last]

-- PART TWO

digitsList : List ((List Char), Char)
digitsList =
    [ (['1'], '1')
    , (['2'], '2')
    , (['3'], '3')
    , (['4'], '4')
    , (['5'], '5')
    , (['6'], '6')
    , (['7'], '7')
    , (['8'], '8')
    , (['9'], '9')
    , (unpack "one", '1')
    , (unpack "two", '2')
    , (unpack "three", '3')
    , (unpack "four", '4')
    , (unpack "five", '5')
    , (unpack "six", '6')
    , (unpack "seven", '7')
    , (unpack "eight", '8')
    , (unpack "nine", '9')
    ]

findFirst : List Char -> Maybe Char
findFirst [] = Nothing
findFirst cs@(x :: xs) =
    case find (\digits => isPrefixOf (fst digits) cs) digitsList of
        Nothing => findFirst xs
        (Just y) => Just (snd y)

findLast : List Char -> Maybe Char
findLast [] = Nothing
findLast cs@(x :: xs) =
    case find (\digits => isSuffixOf (fst digits) (reverse cs)) digitsList of
        Nothing => findLast xs
        (Just y) => Just (snd y)

partTwo : List Char -> Maybe Integer
partTwo chars = do
    first <- findFirst chars
    last <- findLast (reverse chars)
    parseInteger $ pack [first, last]

-- Main

doExercise : (List Char -> Maybe Integer) -> (path : String) -> (msg : String) -> IO ()
doExercise f path msg =
    let
        go : Integer -> File -> IO (Either FileError Integer)
        go k file = do
            False <- fEOF file | True => pure (Right k)
            Right line <- fGetLine file
                | Left err => pure (Left err)
            go (k + (fromMaybe 0 $ f $ unpack line)) file
    in do
        result <- withFile path Read pure (go 0)
        case result of
            (Left err) => (printLn err)
            (Right n) => printLn $ msg ++ (cast n)

main : IO ()
main =
    do 
        doExercise partOne "src/day01/test1.txt" "Part I test result: "
        doExercise partOne "src/day01/input.txt" "Part I result: "
        doExercise partTwo "src/day01/test2.txt" "Part II test result: "
        doExercise partTwo "src/day01/input.txt" "Part II result: "
