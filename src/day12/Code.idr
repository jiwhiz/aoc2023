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

data Code = O | D | U

Show Code where
    show c = case c of O => "."; D => "#"; U => "?"

parseCode : Char -> Code
parseCode c = case c of '.' => O; '#' => D; _ => U 

parseNumbers : String -> List Nat
parseNumbers str = reverse $ 
    foldl
        (\acc, elm =>
            case parsePositive elm of
                Just v => v :: acc
                Nothing => acc
        )
        []
        (forget $ split (',' ==) str)

parseLine : String -> (List Code, List Nat)
parseLine str = bimap (\codes => parseCode <$> unpack codes) (parseNumbers) $ break isSpace (trim str)

group : List Code -> List Nat
group codes = walk 0 codes
    where
        walk : Nat -> List Code -> List Nat
        walk acc [] = if acc > 0 then [acc] else []
        walk acc (x :: xs) =
            case x of
                O => if acc > 0 then acc :: walk 0 xs else walk 0 xs 
                _ => walk (S acc) xs 

arrange : List Code -> List (List Code)
arrange [] = [[]]
arrange (x::xs) =
    case x of
        U => ((O::) <$> arrange xs ) ++ ((D::) <$> arrange xs)
        code => (code::) <$> arrange xs

algo1 : List Code -> List Nat -> Nat 
algo1 codes numbers = length $ filter (== numbers) (group <$> arrange codes)

algo2 : List Code -> List Nat -> Nat 
algo2 codes numbers = backtrack 0 codes numbers 0
    where
        backtrack : Nat -> List Code -> List Nat -> Nat -> Nat
        backtrack k [] [] Z = 1 + k
        backtrack k [] [] (S _) = k
        backtrack k [] (x::xs) acc = if acc == x then backtrack k [] xs 0 else k
        backtrack k (x :: xs) [] Z =
            case x of 
                D => k
                _ => backtrack k xs [] Z 
        backtrack k (x :: xs) [] (S _) = k
        backtrack k (x :: xs) (y :: ys) acc =
            case x of
                O => if (acc == 0) then backtrack k xs (y::ys) 0
                    else if (acc == y) then backtrack k xs ys 0 else k
                D => if (acc < y) then backtrack k xs (y::ys) (S acc) else k
                U => backtrack (backtrack k xs (y::ys) (S acc)) (O::xs) (y::ys) acc


mutual
    go
        : {l, m : Nat}
        -> (map : SortedMap (Nat, Nat) Nat)
        -> (codes : Vect l Code)
        -> (groups : Vect m Nat)
        -> (acc : Nat)
        -> (Nat, SortedMap (Nat, Nat) Nat)
    go map [] [] Z = (1, map)
    go map [] [] (S _) = (0, map)
    go map [] (x::xs) acc = if acc == x then go map [] xs Z else (0, map)
    go map (x :: xs) [] Z =
        case x of
            D => (0, map)
            _ => go map xs [] Z
    go map (x :: xs) [] (S _) = (0, map)
    go map codes@(x :: xs) groups@(y :: ys) acc =
        case x of
            O => if (acc == 0) then checkCache map xs groups
                 else if (acc == y) then checkCache map xs ys
                      else (0, map)
            D => if (acc < y) then go map xs groups (S acc)
                 else (0, map)
            U => let (sumForD, updatedMap) := go map (D::xs) groups acc
                 in mapFst (+ sumForD) $ go updatedMap (O::xs) groups acc

    checkCache
         : {l, m : Nat}
        -> (map : SortedMap (Nat, Nat) Nat)
        -> (codes : Vect l Code)
        -> (groups : Vect m Nat)
        -> (Nat, SortedMap (Nat, Nat) Nat)
    checkCache map codes groups = 
        case SortedMap.lookup (l, m) map of
            Nothing => 
                let (sum, updatedMap) := go map codes groups Z
                in (sum, insert (l, m) sum updatedMap)
            Just count => (count, map)

algo3 : {l, m : Nat} -> Vect l Code -> Vect m Nat -> Nat 
algo3 codes numbers = fst $ go SortedMap.empty codes numbers 0


calculate : List (List Code, List Nat) -> Nat
calculate lines =
    foldl 
        (\acc, (codes, numbers) => acc + algo3 (fromList codes) (fromList numbers))
        0
        lines


unfoldCode : List Code -> List Code
unfoldCode x = x ++ [U] ++ x ++ [U] ++ x ++ [U] ++ x ++ [U] ++ x

unfoldNum : List Nat -> List Nat
unfoldNum x = x ++ x ++ x ++ x ++ x

covering
main : IO ()
main =
    do 
        lines <- Util.parseFile "src/day12/input.txt"
        let codeLines := (filter (\str => length str > 0) lines)
        printLn $ "Part I result: " ++ (show $ calculate $ parseLine <$> codeLines)
        printLn $ "Part II result: " ++ (show $ calculate $ (bimap unfoldCode unfoldNum . parseLine) <$> codeLines)
        pure ()
