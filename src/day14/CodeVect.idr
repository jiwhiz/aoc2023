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

tilt : {n,m:Nat} -> Vect m Char -> Vect n Char -> Vect (m+n) Char
tilt {n=Z}     {m} acc [] = rewrite plusZeroRightNeutral m in acc
tilt {n=(S k)} {m} acc ('.'::xs) = rewrite sym $ plusSuccRightSucc m k in tilt ('.'::acc) xs
tilt {n=(S k)} {m} acc ('O'::xs) = rewrite sym $ plusSuccRightSucc m k in 'O'::tilt [] (acc ++ xs)
tilt {n=(S k)} {m} acc (x::xs) = acc ++ (x :: tilt [] xs)

Board : Nat -> Nat -> Type
Board j k = Vect j (Vect k Char)

rollNorth : {j,k:Nat} -> Board j k -> Board j k
rollNorth = transpose . (tilt [] <$>) . transpose

rollWest : {j,k:Nat} -> Board j k -> Board j k
rollWest = (<$>) (tilt [])

rollSouth : {j,k:Nat} -> Board j k -> Board j k
rollSouth = transpose . (reverse <$>) . (tilt [] <$>) . (reverse <$>). transpose

rollEast : {j,k:Nat} -> Board j k -> Board j k
rollEast = (reverse <$>) . (tilt [] <$>) . (reverse <$>)

cycle : {j,k:Nat} -> Board j k -> Board j k
cycle = rollEast . rollSouth . rollWest . rollNorth

northLoad : {j,k:Nat} -> Board j k -> Nat
northLoad = sum . (count <$>) . transpose
    where
        count : Vect n Char -> Nat
        count [] = 0
        count l@('O'::xs) = length l + count xs
        count (x::xs) = count xs

runSpin : {j,k:Nat} -> Board j k -> (n ** (Fin n, Vect n (Board j k)))
runSpin start = run start [start] 
    where
        run : {j,k,n:Nat} -> Board j k -> Vect n (Board j k) -> (m ** (Fin m, Vect m (Board j k)))
        run b bList =
            let newB := cycle b 
            in case findIndex (== newB) bList of
                Nothing => run newB (bList ++ [newB])
                Just index => (n ** (index, bList))

getBoard : (n ** (Fin n, Vect n (Board j k))) -> Maybe (Board j k)
getBoard (len ** (idx, blist)) =
    let idxInt = finToInteger idx
        bidxInt := idxInt + mod (1000000000 - idxInt) (cast len - idxInt)
    in case (integerToFin bidxInt len) of
        Just bidx => Just $ index bidx blist
        Nothing => Nothing

parseBoard : (m : Nat) -> (lines : List String) -> List (Vect m Char)
parseBoard _ [] = []
parseBoard m (l :: ls) =
    let chars := unpack $ trim l in
    case decEq (length chars) m of
        (Yes Refl) => (fromList chars) :: parseBoard m ls
        (No _) => parseBoard m ls

covering
main : IO ()
main =
    do
        (l :: lines) <- Util.parseFile "src/day14/input.txt" | [] => printLn "Error parse file" 
        let board := fromList $ parseBoard (length l) (l::lines)
        printLn $ "Part I result: " ++ (show . northLoad . rollNorth) board

        let Just finalBoard := getBoard $ runSpin board | Nothing => printLn "Error to get final board"
        printLn $ "Part II result: " ++ (show . northLoad) finalBoard
        pure ()
