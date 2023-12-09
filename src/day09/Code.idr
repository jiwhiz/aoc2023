module Main

import Data.List1
import Data.Maybe
import Data.Vect
import Util

%default total

sameValue : Integer -> Vect _ Integer -> Bool
sameValue i [] = True
sameValue i (y :: ys) = if i /= y then False else sameValue i ys

diff : {k: Nat} -> Vect (S (S k)) Integer -> Vect (S k) Integer
diff (x1 :: [x2]) = [x2-x1]
diff (x1 :: x2 :: x3 :: xs) = (x2 - x1) :: diff (x2 :: x3 :: xs)

extrapolate
     : {n : Nat}
    -> ({m : Nat} -> Vect (S m) Integer -> Integer -> Integer)
    -> Vect (S (S n)) Integer
    -> Maybe Integer
extrapolate {n=Z} _ (x1 :: [x2]) = if x1 == x2 then Just x1 else Nothing
extrapolate {n=(S k)} f input@(x1 :: x2 :: x3 :: xs) = 
    if sameValue x1 (x2 :: x3 :: xs) then Just x1 
    else do
        value <- extrapolate f $ diff input
        pure $ f {m=(S (S k))} input value

calculate : ({m : Nat} -> Vect (S m) Integer -> Integer -> Integer) -> List String -> Integer
calculate f lines =
    foldl (\acc, line => acc + fromMaybe 0 (handle line)) 0 lines
    where
        handle : String -> Maybe Integer
        handle str =
            case parseIntegers str of
                l@(l1 :: l2 :: ls) => do
                    numbers <- toVect (S (S (length ls))) l
                    extrapolate {n=length ls} f numbers
                _ => Nothing

f1 : {m : Nat} -> Vect (S m) Integer -> Integer -> Integer
f1 list value = value + (head . reverse) list

f2 : {m : Nat} -> Vect (S m) Integer -> Integer -> Integer
f2 list value = head list - value

covering
main : IO ()
main =
    do 
        lines <- Util.parseFile "src/day09/input.txt"
        printLn $ "Part I result: " ++ (show $ calculate f1 lines)
        printLn $ "Part II result: " ++ (show $ calculate f2 lines)
