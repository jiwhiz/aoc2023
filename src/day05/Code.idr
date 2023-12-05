module Main

import Data.String
import Data.List
import System.File


record AMap where
    constructor MkAMap
    desStart : Integer
    srcStart : Integer
    range : Integer


mapNum : Integer -> AMap -> Maybe Integer
mapNum src m =
    if src >= m.srcStart && src < m.srcStart + m.range then Just (src + m.desStart - m.srcStart) else Nothing


mapNumber : Integer -> List AMap -> Integer
mapNumber src [] = src
mapNumber src (x :: xs) =
    case mapNum src x of
        Nothing => mapNumber src xs
        (Just y) => y

mapChain : Integer -> List (List AMap) -> Integer
mapChain src [] = src
mapChain src (x :: xs) = mapChain (mapNumber src x) xs

parseAMap : String -> Maybe AMap
parseAMap str =
    case words str of
        s1 :: s2 :: s3 :: [] => do
            desStart <- parseInteger s1
            srcStart <- parseInteger s2
            range <- parseInteger s3 
            pure $ MkAMap desStart srcStart range
        _ => Nothing

parseSeeds : String -> List Integer
parseSeeds str =
    foldl
        (\acc, elm =>
            case parseInteger elm of
                Just v => v :: acc
                Nothing => acc
        )
        []
        (words str)

parseFile : (path : String) -> IO (List Integer, List (List AMap))
parseFile path =
    let
        go : List AMap -> (List Integer, List (List AMap)) -> File -> IO (Either FileError (List Integer, List (List AMap)))
        go accMap r@(seeds, maps) file = do
            False <- fEOF file | True => pure (Right r)
            Right line <- fGetLine file
                | Left err => pure (Left err)
            if isPrefixOf "seeds: " line
                then go [] (reverse $ parseSeeds $ snd $ break (isSpace) line, []) file
                else case parseAMap line of
                    Nothing => go [] (seeds, accMap :: maps) file 
                    Just map => go (map :: accMap) (seeds, maps) file
    in do
        result <- withFile path Read pure (go [] ([], []))
        case result of
            (Left err) => pure ([], [])
            (Right n) => pure n

-- PART I

minInteger : List Integer -> Integer
minInteger is =
    foldl (\r, i => min r i) 99999999999999 is

calculate1 : List Integer -> List (List AMap) -> Integer
calculate1 seeds chains = minInteger $
    foldl (\acc, seed => (mapChain seed chains) :: acc) [] seeds


-- PART II

pairSeeds : List Integer -> List (Integer, Integer)
pairSeeds (x :: y :: xs) = (x, y) :: pairSeeds xs
pairSeeds _ = []

calRange : List Integer -> Integer -> Integer -> List (List AMap) -> List Integer
calRange acc seed 0 chains = acc
calRange acc seed range chains = calRange ((mapChain seed chains) :: acc) (seed +1) (range - 1) chains

calculate2 : List (Integer, Integer) -> List (List AMap) -> Integer
calculate2 seeds chains = minInteger $
    foldl (\acc, (seed, range) => (minInteger $ calRange [] seed range chains) :: acc) [] seeds

-- Main

prn : List Integer -> String
prn [] = ""
prn (x::xs) = (cast x) ++ "," ++ prn xs

main : IO ()
main = do
    (seeds, mapsChain) <- parseFile "src/day05/input.txt"
    -- printLn $ "Seeds: " ++ prn seeds
    printLn $ "I Lowest location number: " ++ cast (calculate1 seeds (reverse mapsChain))
    printLn $ "II Lowest location number: " ++ cast (calculate2 (pairSeeds $ seeds) (reverse mapsChain))
