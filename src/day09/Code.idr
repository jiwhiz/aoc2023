module Main

import Data.Vect
import Util

%default total

||| calculate the difference values of the sequence
diff : Vect (S k) Integer -> Vect k Integer
diff [x] = []
diff (x1 :: x2 :: xs) = (x2 - x1) :: diff (x2 :: xs)

||| extrapolated value of each line is the last numebr plus the extrapolated value of diff sequence
extrapolate : Vect n Integer -> Integer
extrapolate [] = 0
extrapolate input@(x::xs) = (head . reverse) input + extrapolate (diff input)

covering
main : IO ()
main =
    do 
        lines <- Util.parseFile "src/day09/input.txt"
        printLn $ "Part I result: " ++ 
            (show $ sum <$> (traverse ((\l => extrapolate <$> (toVect (length l) l)) . parseIntegers) lines))
        printLn $ "Part II result: " ++
            (show $ sum <$> (traverse ((\l => extrapolate <$> (toVect (length l) l)) . reverse . parseIntegers) lines))
