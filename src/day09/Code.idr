module Main

import Data.List
import Data.List1
import Data.Maybe
import Data.Vect
import Util

%default total

diff : {k: Nat} -> Vect (S k) Integer -> Vect k Integer
diff {k=Z} [x] = []
diff {k=S Z} (x1 :: [x2]) = [x2-x1]
diff {k=S m} (x1 :: x2 :: xs) = (x2 - x1) :: diff {k=m} (x2 :: xs)

extrapolate : {n : Nat} -> Vect n Integer -> Integer
extrapolate {n=Z} [] = 0
extrapolate {n=S m} input@(x::xs) = (head . reverse) input + extrapolate (diff {k=m} input)

covering
main : IO ()
main =
    do 
        lines <- Util.parseFile "src/day09/input.txt"
        printLn $ "Part I result: " ++ 
            (show $ sum <$> (traverse ( ( (\l => extrapolate <$> (toVect (length l) l)) . parseIntegers) ) lines))
        printLn $ "Part II result: " ++
            (show $ sum <$> (traverse ( ( (\l => extrapolate <$> (toVect (length l) l)) . reverse . parseIntegers) ) lines))
