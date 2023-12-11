module Main

import Debug.Trace
import Data.Fin
import Data.List
import Data.String
import Data.Vect
import Decidable.Equality
import Util

data Cell = S | G

Show Cell where
    show c = case c of S => "."; G => "#"

parseCell : Char -> Cell
parseCell c = case c of '#' => G; _ => S 

parseBoard : (m : Nat) -> (lines : List String) -> List (List Cell)
parseBoard _ [] = []
parseBoard m (l :: ls) =
    let chars := unpack $ trim l in
    if length chars == m
    then (parseCell <$> chars) :: parseBoard m ls
    else parseBoard m ls

allSpace : List Cell -> Bool 
allSpace [] = True
allSpace (x :: xs) = case x of S => allSpace xs; G => False

printBoard : List (List Cell) -> IO ()
printBoard [] = do pure ()
printBoard (x :: xs) = do
    printLn $ joinBy "" $ show <$> x
    printBoard xs


||| Get sum of the distances to all G nodes in belowing rows
countBelow : Nat -> Nat -> Nat -> List Nat -> List (List Cell) -> Nat
countBelow _ _ _ _ [] = 0
countBelow ratio startCol rowDistance colRatios (x :: xs) = 
    let left := reverse $ take startCol x
        right := drop startCol x
        rowRatio := if allSpace x then ratio else 1
        leftColRatios := reverse $ take startCol colRatios
        rightColRatios := drop startCol colRatios
    in  countDistance leftColRatios left 1 + countDistance rightColRatios right 0 + countBelow ratio startCol (rowDistance + rowRatio) colRatios xs
    where
        countDistance : List Nat -> List Cell -> Nat -> Nat
        countDistance _ [] _ = 0
        countDistance [] _ _ = 0
        countDistance (colRatio :: remainingRatios) (y :: ys) colDistance =
            case y of
                S => countDistance remainingRatios ys (colDistance + colRatio)
                G => colDistance + rowDistance + countDistance remainingRatios ys (colDistance + colRatio)


cal : Nat -> List Nat -> List (List Cell) -> Nat
cal ratio _ [] = 0
cal ratio colRatios (x :: xs) =
    scanRow 0 colRatios x xs + cal ratio colRatios xs
    where
        ||| Get sum of the distances to all G nodes on the right side of the same row
        countRight : Nat -> List Nat -> List Cell -> Nat
        countRight k [] _ = 0
        countRight k _ [] = 0
        countRight k (colRatio :: remainingRatios) (x :: xs) =
            case x of
                S => countRight (k + colRatio) remainingRatios xs
                G => k + countRight (k + colRatio) remainingRatios xs

        ||| scan G nodes of current row to get sum of lengths to all other G nodes on right and below
        scanRow : Nat -> List Nat -> List Cell -> List (List Cell) -> Nat
        scanRow _ _ [] _ = 0
        scanRow _ [] _ _ = 0
        scanRow pos (colRatio :: remainingRatios) (y :: ys) below =
            case y of
                S => scanRow (pos + 1) remainingRatios ys below
                G => countRight colRatio remainingRatios ys +  -- sum of lengths to all G nodes at right side
                     countBelow ratio pos (if allSpace x then ratio else 1) colRatios below + -- sum of lengths to all G nodes below
                     scanRow (pos + 1) remainingRatios ys below -- continue scanning remaining row


expandRatio : Nat -> List (List Cell) -> List Nat
expandRatio ratio [] = []
expandRatio ratio (x :: xs) = if allSpace x then ratio :: expandRatio ratio xs else 1 :: expandRatio ratio xs


covering
main : IO ()
main =
    do 
        (l :: lines) <- Util.parseFile "src/day11/input.txt" | [] => printLn "Error parse file"
        let board := parseBoard (length $ trim l) (l::lines)       
        printLn $ "Part I result: " ++ (show $ cal 2 (expandRatio 2 (transpose board)) board)
        printLn $ "Part II result: " ++ (show $ cal 1000000 (expandRatio 1000000 (transpose board)) board)
        pure ()
