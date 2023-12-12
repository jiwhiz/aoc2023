module Main

import Data.List
import Data.Nat
import Data.String
import Data.Vect
import Decidable.Equality
import Util

data Cell = S | G

Show Cell where
    show c = case c of S => "."; G => "#"

parseCell : Char -> Cell
parseCell c = case c of '#' => G; _ => S 

parseBoard : (m : Nat) -> (lines : List String) -> List (Vect m Cell)
parseBoard _ [] = []
parseBoard m (l :: ls) =
    let chars := unpack $ trim l in
    case decEq (length chars) m of
        (Yes Refl) => (parseCell <$> fromList chars) :: parseBoard m ls
        (No _) => parseBoard m ls


allSpace : Vect _ Cell -> Bool 
allSpace [] = True
allSpace (x :: xs) = case x of S => allSpace xs; G => False


||| Get sum of the lengths to all G nodes in belowing rows
countBelow : {n,l:Nat} -> Nat -> (startCol : Nat) -> Nat -> Vect (startCol + l) Nat -> Vect n (Vect (startCol + l) Cell) -> Nat
countBelow _ _ _ _ [] = 0
countBelow ratio startCol rowDistance colRatios (x :: xs) = 
    let left := reverse $ take startCol x
        right := drop startCol x
        rowRatio := if allSpace x then ratio else 1
        leftColRatios := reverse $ take startCol colRatios
        rightColRatios := drop startCol colRatios
    in  countDistance leftColRatios left 1 + countDistance rightColRatios right 0 + countBelow ratio startCol (rowDistance + rowRatio) colRatios xs
    where
        countDistance : Vect k Nat -> Vect k Cell -> Nat -> Nat
        countDistance [] [] _ = 0
        countDistance (colRatio :: remainingRatios) (y :: ys) colDistance =
            case y of
                S => countDistance remainingRatios ys (colDistance + colRatio)
                G => colDistance + rowDistance + countDistance remainingRatios ys (colDistance + colRatio)


cal : {n,m:Nat} -> Nat -> Vect m Nat -> Vect n (Vect m Cell) -> Nat
cal _ _ [] = 0
cal ratio colRatios (curRow :: remRows) =
    scanRow 0 {prf=plusZeroLeftNeutral m} colRatios curRow + cal ratio colRatios remRows
    where
        ||| Get sum of the distances to all G nodes on the right side of the same row
        countRight : Nat -> Vect len Nat -> Vect len Cell -> Nat
        countRight k [] [] = 0
        countRight k (colRatio :: remainingRatios) (x :: xs) =
            case x of
                S => countRight (k + colRatio) remainingRatios xs
                G => k + countRight (k + colRatio) remainingRatios xs

        ||| scan G nodes of current row to get sum of lengths to all other G nodes on right and below
        scanRow : {len:Nat} -> (pos : Nat) -> {prf:pos+len=m} -> Vect len Nat -> Vect len Cell -> Nat
        scanRow _ [] [] = 0
        scanRow {len=S k} pos {prf} (colRatio :: remainingRatios) (y :: ys) =
            case y of
                S => scanRow (S pos) {prf=rewrite plusSuccRightSucc pos k in prf} remainingRatios ys
                G => countRight colRatio remainingRatios ys  -- sum of lengths to all G nodes at right side
                    + countBelow {l=S k} ratio pos (if allSpace curRow then ratio else 1) (rewrite prf in colRatios) (rewrite prf in remRows)
                    + scanRow (S pos) {prf=rewrite plusSuccRightSucc pos k in prf} remainingRatios ys -- continue scanning remaining row

expandRatio : Nat -> Vect m (Vect _ Cell) -> Vect m Nat
expandRatio ratio [] = []
expandRatio ratio (x :: xs) = if allSpace x then ratio :: expandRatio ratio xs else 1 :: expandRatio ratio xs


covering
main : IO ()
main =
    do 
        (l :: lines) <- Util.parseFile "src/day11/input.txt" | [] => printLn "Error parse file"
        let board := fromList $ parseBoard (length l) (l::lines)       
        printLn $ "Part I result: " ++ (show $ cal 2 (expandRatio 2 (transpose board)) board)
        printLn $ "Part II result: " ++ (show $ cal 1000000 (expandRatio 1000000 (transpose board)) board)
        pure ()
