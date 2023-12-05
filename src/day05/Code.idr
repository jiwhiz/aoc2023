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
        go accMap (seeds, maps) file = do
            False <- fEOF file | True => pure (Right (reverse seeds, reverse maps))
            Right line <- fGetLine file
                | Left err => pure (Left err)
            if isPrefixOf "seeds: " line
                then go [] (parseSeeds $ snd $ break (isSpace) line, []) file
                else case parseAMap line of
                    Nothing => go [] (seeds, (if isNil accMap then maps else accMap :: maps)) file 
                    Just map => go (map :: accMap) (seeds, maps) file
    in do
        result <- withFile path Read pure (go [] ([], []))
        case result of
            (Left err) => pure ([], [])
            (Right n) => pure n

-- PART I

calculate1 : Integer -> List Integer -> List (List AMap) -> Integer
calculate1 minResult seeds chains =
    foldl (\acc, seed => min (mapChain seed chains) acc) minResult seeds

partOne  : List Integer -> List (List AMap) -> Integer
partOne seeds chains = calculate1 99999999999 seeds chains


-- PART II

pairSeeds : List Integer -> List (Integer, Integer)
pairSeeds (x :: y :: xs) = (x, y) :: pairSeeds xs
pairSeeds _ = []

calRange : Integer -> Integer -> Integer -> List (List AMap) -> Integer
calRange minResult seed 0 chains = minResult
calRange minResult seed range chains = calRange (min (mapChain seed chains) minResult) (seed +1) (range - 1) chains


calculate2 : Integer -> List (Integer, Integer) -> List (List AMap) -> Integer
calculate2 minResult seeds chains = 
    foldl (\acc, (seed, range) => min (calRange acc seed range chains) acc) minResult seeds

partTwo : List Integer -> List (List AMap) -> Integer
partTwo seedRanges mapChains = calculate2 99999999999 (pairSeeds seedRanges) mapChains

-- Main

main : IO ()
main = do
    (seeds, mapsChain) <- parseFile "src/day05/input.txt"
    printLn $ "Part  I Lowest location number: " ++ cast (partOne seeds mapsChain)
    printLn $ "Part II Lowest location number: " ++ cast (partTwo seeds mapsChain)
