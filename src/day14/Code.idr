module Main

import Debug.Trace
import Data.Fin
import Data.List
import Data.List1
import Data.SortedMap
import Data.String
import Data.Vect
import Decidable.Equality
import Util

tilt : List Char -> List Char -> List Char
tilt acc [] = acc
tilt acc ('.' :: xs) = tilt ('.'::acc) xs
tilt acc ('O' :: xs) = 'O':: tilt [] (acc ++ xs)
tilt acc (x :: xs) = acc ++ [x] ++ (tilt [] xs)

Board : Type
Board = List (List Char)

rollNorth : Board -> Board
rollNorth = transpose . (tilt [] <$>) . transpose

rollWest : Board -> Board
rollWest = (<$>) (tilt [])

rollSouth : Board -> Board
rollSouth = transpose . (reverse <$>) . (tilt [] <$>) . (reverse <$>). transpose

rollEast : Board -> Board
rollEast = (reverse <$>) . (tilt [] <$>) . (reverse <$>)

cycle : Board -> Board
cycle = rollEast . rollSouth . rollWest . rollNorth

northLoad : Board -> Nat
northLoad = sum . (count <$>) . transpose
    where
        count : List Char -> Nat
        count [] = 0
        count l@('O'::xs) = length l + count xs
        count (x::xs) = count xs

runSpin : Board -> (n ** (Fin n, Vect n Board))
runSpin start = run start [start] 
    where
        run : {n:Nat} -> Board -> Vect n Board -> (m ** (Fin m, Vect m Board))
        run b bList =
            let newB := cycle b 
            in case findIndex (== newB) bList of
                Nothing => run newB (bList ++ [newB])
                Just index => (n ** (index, bList))

getBoard : (n ** (Fin n, Vect n Board)) -> Maybe Board
getBoard (len ** (idx, blist)) =
    let idxInt = finToInteger idx
        bidxInt := idxInt + mod (1000000000 - idxInt) (cast len - idxInt)
    in case (integerToFin bidxInt len) of
        Just bidx => Just $ index bidx blist
        Nothing => Nothing

partial
main : IO ()
main =
    do 
        lines <- Util.parseFile "src/day14/input.txt"
        let board := unpack <$> (filter (\str => length str > 0) lines)
        printLn $ "Part I result: " ++ (show . northLoad . rollNorth) board

        let (len ** (idx, bList)) := runSpin board
            Just finalBoard := getBoard $ runSpin board | Nothing => printLn "Error to get final board"
        printLn $ "Part II result: " ++ (show . northLoad) finalBoard
        pure ()
