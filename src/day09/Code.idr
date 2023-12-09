module Main

import Data.List1
import Data.Maybe
import Util

%default total

diff : List Integer -> Maybe (List Integer)
diff (x1 :: [x2]) = Just [x2-x1]
diff (x1 :: x2 :: xs) = do 
    tail <- diff (x2 :: xs)
    pure $ (x2 - x1) :: tail
diff _ = Nothing

sameValue : Integer -> List Integer -> Bool
sameValue i [] = True
sameValue i (y :: ys) = if i /= y then False else sameValue i ys

partial
calculate : (List1 Integer -> Integer -> Integer) -> List Integer -> Maybe Integer
calculate f [] = Nothing
calculate f input@(x :: xs) =
    if sameValue x xs then Just x
    else do
        nextSeq <- diff input
        value <- calculate f nextSeq 
        pure $ f (x:::xs) value

f1 : List1 Integer -> Integer -> Integer
f1 list value = value + (head $ reverse list)

partial
partOne : List String -> Integer
partOne lines = foldl (\acc, line => acc + fromMaybe 0 (calculate f1 $ parseIntegers line)) 0 lines

f2 : List1 Integer -> Integer -> Integer
f2 input value = head input - value

partial
partTwo : List String -> Integer
partTwo lines = foldl (\acc, line => acc + fromMaybe 0 (calculate f2 $ parseIntegers line)) 0 lines

-- -- Main
covering
main : IO ()
main =
    do 
        lines <- Util.parseFile "src/day09/input.txt"
        printLn $ "Part I result: " ++ (show $ partOne lines)
        printLn $ "Part II result: " ++ (show $ partTwo lines)
