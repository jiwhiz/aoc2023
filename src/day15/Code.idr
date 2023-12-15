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

%default total

hash : String -> Fin 256
hash str =
    case doHash (unpack str) 0 of
        Nothing => 0
        Just value => value
    where
        doHash : List Char-> Fin 256 -> Maybe (Fin 256)
        doHash [] v = Just v
        doHash (x :: xs) v = do
            next <- integerToFin (mod ((finToInteger v + cast (ord x)) * 17) 256) 256
            doHash xs next

partOne : String -> Integer
partOne str =
    foldl (\acc, s => acc + finToInteger (hash s)) 0 $ forget $ split (== ',') str

record Lens where
    constructor MkLens
    label : String
    focalLength : Integer

Show Lens where
    show lens =
        lens.label ++ "=" ++ (cast lens.focalLength)

process : String -> Vect 256 (List Lens) -> Vect 256 (List Lens)
process step boxes =
    let (l, sign) := break (== '-') step in
    if sign == "-" then remove l
    else 
        reset $ break (== '=') step
    where
        remove : String -> Vect 256 (List Lens)
        remove label =
            let
                idx := hash label
                lensList := index idx boxes
            in case findIndex (\lens => lens.label == label) lensList of
                Nothing => boxes
                Just lensIdx => replaceAt idx (deleteBy (\l, lens => lens.label == l) label lensList) boxes

        reset : (String, String) -> Vect 256 (List Lens)
        reset (label, focalStr) =
            let 
                idx := hash label
                lensList := index idx boxes
                focalLength := fromMaybe 0 $ parseInteger $ pack $ dropWhile (== '=') $ unpack focalStr
            in case findIndex (\lens => lens.label == label) lensList of
                Nothing => replaceAt idx (lensList ++ [MkLens label focalLength]) boxes
                Just lensIdx => replaceAt idx (replaceWhen (\lens => lens.label == label) (MkLens label focalLength) lensList) boxes

calBoxes : Integer -> Vect _ (List Lens) -> Integer
calBoxes _ [] = 0
calBoxes index (box::boxes) = calLens index 1 box + calBoxes (index + 1) boxes
    where
        calLens : Integer -> Integer -> List Lens -> Integer
        calLens boxIndex lensIndex [] = 0
        calLens boxIndex lensIndex (lens::lensList) = (boxIndex * lensIndex * lens.focalLength)
            + calLens boxIndex (lensIndex + 1) lensList

partTwo : String -> Integer
partTwo str =
    calBoxes 1 $
        foldl (\boxes, s => process s boxes) (replicate 256 []) $ forget $ split (== ',') str

covering
main : IO ()
main =
    do
        (l :: lines) <- Util.parseFile "src/day15/input.txt" | [] => printLn "Error parse file" 
        printLn $ "Part I result: " ++ (show $ partOne l)
        printLn $ "Part II result: " ++ (show $ partTwo l)
        pure ()
