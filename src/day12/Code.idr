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
parseLine str = bimap (\codes => parseCode <$> unpack codes) (parseNumbers) $ break isSpace str

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
        : {len, l : Nat}
        -> {auto prf: LT l len}
        -> Vect len (SortedMap (List Nat) Nat)
        -> (codes : Vect l Code)
        -> (groups : List Nat)
        -> (k : Nat)
        -> (acc : Nat)
        -> (Nat, Nat, Vect len (SortedMap (List Nat) Nat))
    go maps [] [] k Z = ((S k), 1, maps)
    go maps [] [] k (S _) = (k, 0, maps)
    go maps [] (x::xs) k acc = if acc == x then checkCache maps [] xs k Z else (k, 0, maps)
    go maps (x :: xs) [] k Z =
        case x of
            D => (k, 0, maps)
            _ => checkCache maps xs {prf=lteSuccLeft prf} [] k Z
    go maps (x :: xs) [] k (S _) = (k, 0, maps)
    go maps codes@(x :: xs) groups@(y :: ys) k acc =
        case x of
            O => if (acc == 0) then checkCache maps xs {prf=lteSuccLeft prf} (y::ys) k Z
                 else if (acc == y) then checkCache maps xs {prf=lteSuccLeft prf} ys k Z
                      else (k, 0, maps)
            D => if (acc < y) then go maps xs {prf=lteSuccLeft prf} (y::ys) k (S acc)
                 else (k, 0, maps)
            U => let (sum, _, updatedMaps) := 
                    if acc == 0 then checkCache maps (D::xs) (y::ys) k Z 
                        else go maps xs {prf=lteSuccLeft prf} (y::ys) k (S acc)
                 in if acc == 0 then checkCache updatedMaps (O::xs) (y::ys) sum Z 
                    else go updatedMaps (O::xs) (y::ys) sum acc

    checkCache
        : {len, l : Nat}
        -> {auto prf: LT l len}
        -> Vect len (SortedMap (List Nat) Nat)
        -> (codes : Vect l Code)
        -> (groups : List Nat)
        -> (k : Nat)
        -> (acc : Nat)
        -> (Nat, Nat, Vect len (SortedMap (List Nat) Nat))
    checkCache maps codes groups k acc = 
        let map := index (natToFinLT l) maps
        in case SortedMap.lookup groups map of
            Nothing => 
                let (sum, subtotal, updatedMaps) := go maps codes groups k acc
                    updatedMap := index (natToFinLT l) updatedMaps
                in
                    if (subtotal > 0) then (sum, subtotal, replaceAt (natToFinLT l) (insert groups subtotal updatedMap) updatedMaps)
                    else (sum, subtotal, updatedMaps)
            Just count => ((count+k), count, maps)

algo3 : {len:Nat} -> Vect len Code -> List Nat -> Nat 
algo3 codes numbers = fst $ go (initMaps (S len)) codes {prf=proofLt len} numbers 0 0
    where
        initMaps : (l : Nat) -> Vect l (SortedMap (List Nat) Nat)
        initMaps l = replicate l SortedMap.empty

        proofLt : (l : Nat) -> LT l (S l)
        proofLt Z = LTESucc LTEZero
        proofLt (S k) = LTESucc (proofLt k)


calculate : List (List Code, List Nat) -> Nat
calculate lines =
    foldl 
        (\acc, (codes, numbers) => acc + algo3 (fromList codes) numbers)
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
        printLn $ "Part I result: " ++ (show $ calculate $ parseLine <$> (lines))
        printLn $ "Part II result: " ++ (show $ calculate $ (bimap unfoldCode unfoldNum . parseLine) <$> lines)
        pure ()
