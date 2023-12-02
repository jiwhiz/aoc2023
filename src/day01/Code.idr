module Main

import Data.List
import Data.Maybe
import Data.String

import System.File


parseValue : String -> Integer
parseValue str = 
    let
        chars := unpack str 
        first := fromMaybe ' ' $ find isDigit chars
        last := fromMaybe ' ' $ find isDigit (reverse chars)
        caliString := pack [first, last]
    in
        fromMaybe 0 $ parseInteger caliString 
    
caliValue : String -> Maybe Integer
caliValue str =
    let chars := unpack str in
        do
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
    case find (\digits=> isPrefixOf (fst digits) cs) digitsList of
        Nothing => findFirst xs
        (Just y) => Just (snd y)



findLast : List Char -> Maybe Char
findLast [] = Nothing
findLast cs@(x :: xs) =
    case find (\digits=> isSuffixOf (fst digits) (reverse cs)) digitsList of
        Nothing => findLast xs
        (Just y) => Just (snd y)


caliValue' : String -> Maybe Integer
caliValue' str =
    let chars := unpack str in
        do
            first <- findFirst chars
            last <- findLast (reverse chars)
            parseInteger $ pack [first, last]

    


calibration : (path : String) -> (String -> Maybe Integer) -> IO (Either FileError Integer)
calibration path f = withFile path Read pure (go 0)
  where covering go : Integer -> File -> IO (Either FileError Integer)
        go k file = do
          False <- fEOF file | True => pure (Right k)
          Right line <- fGetLine file
            | Left err => pure (Left err)
          go (k + (fromMaybe 0 $ f line)) file


part1 : IO ()
part1 =
    do
        test <- (calibration "src/day01/test1.txt" caliValue)
        case test of
            (Left err) => (printLn err)
            (Right n) => printLn $ "Part I test result: " ++ (cast n)
        result <- (calibration "src/day01/input.txt" caliValue)
        case result of
            (Left err) => (printLn err)
            (Right n) => printLn $ "Part I result: " ++ (cast n)

part2 : IO ()
part2 =
    do
        test <- (calibration "src/day01/test2.txt" caliValue')
        case test of
            (Left err) => (printLn err)
            (Right n) => printLn $ "Part II test result: " ++ (cast n)
        result <- (calibration "src/day01/input.txt" caliValue')
        case result of
            (Left err) => (printLn err)
            (Right n) => printLn $ "Part II result: " ++ (cast n)


main : IO ()
main =
    do 
        part1
        part2
    
