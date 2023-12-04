module Main

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System.File

parseNumbers : String -> Maybe (List Integer)
parseNumbers input =
    foldlM
        (\results, numStr => do
            num <- parseInteger $ trim numStr
            pure (num :: results)
        )
        []
        (words input)

getMatches : List Integer -> List Integer -> Integer
getMatches wins nums =
    foldl
        (\acc, num =>
            case find (==num) wins of
                Just _ => acc + 1
                Nothing => acc
        )
        0
        nums

||| parse one line of card and return matched numbers count
parseCardLine : String -> Integer
parseCardLine line = 
    let result := do
        numbersLine <- head' $ tail $ split (==':') line
        winNumbers <- parseNumbers $ head $ split (=='|') numbersLine
        mineNumbersStr <- head' $ tail $ split (=='|') numbersLine
        mineNumbers <- parseNumbers mineNumbersStr
        pure (winNumbers, mineNumbers)
    in
        case result of
            Nothing => 0
            Just (wins, nums) => getMatches wins nums

-- PART I

getPoints : Integer -> Integer
getPoints 0 = 0
getPoints 1 = 1
getPoints m = 2 * getPoints (m-1)

partOne : List Integer -> Integer
partOne matches =
    foldl (\acc, m => acc + getPoints m) 0 matches

-- PART II

updateCopies : Integer -> Integer -> List Integer -> List Integer
updateCopies _ _ [] = []
updateCopies 0 cardCopy (x :: xs) = x :: xs
updateCopies matches cardCopy (x :: xs) = cardCopy + x :: updateCopies (matches - 1) cardCopy xs


calculate : List Integer -> List Integer -> List Integer
calculate (m :: ms) (x :: xs) = x :: (calculate ms $ updateCopies m x xs)
calculate _ _ = []


initCopies : List Integer -> List Integer
initCopies [] = []
initCopies (x :: xs) = 1 :: initCopies xs


partTwo : List Integer -> Integer
partTwo matches = foldl (+) 0 (calculate matches $ initCopies matches)


-- Main

doExercise : (List Integer -> Integer) -> (path : String) -> (msg : String) -> IO ()
doExercise proc path msg =
    let
        go : List Integer -> File -> IO (Either FileError (List Integer))
        go l file = do
            False <- fEOF file | True => pure (Right l)
            Right line <- fGetLine file
                | Left err => pure (Left err)
            if (strLength line > 0 ) then go (parseCardLine line :: l) file
                else go l file
    in do
        result <- withFile path Read pure (go [])
        case result of
            (Left err) => (printLn err)
            (Right matches) => printLn $ msg ++ (cast $ proc $ reverse matches)

main : IO ()
main = do
    doExercise partOne "src/day04/test.txt" "Part I test result: "
    doExercise partOne "src/day04/input.txt" "Part I result: "
    doExercise partTwo "src/day04/test.txt" "Part II test result: "
    doExercise partTwo "src/day04/input.txt" "Part II result: "
