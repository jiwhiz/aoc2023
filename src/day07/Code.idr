module Main

import Data.List
import Data.String
import Data.Vect
import System
import System.File

data HandType = FiveOfAKind | FourOfAKind | FullHouse | ThreeOfAKind | TwoPair | OnePair | HighCard

typeRank : HandType -> Nat
typeRank FiveOfAKind = 6
typeRank FourOfAKind = 5
typeRank FullHouse = 4
typeRank ThreeOfAKind = 3
typeRank TwoPair = 2
typeRank OnePair = 1
typeRank HighCard = 0

implementation Eq HandType where
    t1 == t2 = typeRank t1 == typeRank t2

implementation Ord HandType where
    compare = comparing typeRank

count : List Char -> List Nat
count [] = []
count (card :: remaining) = 
    let rCount := count remaining
        (cardCount, updatedCounts) := checkDup remaining rCount 
    in S cardCount :: updatedCounts
    where
        checkDup : List Char -> List Nat -> (Nat, List Nat)
        checkDup [] _ = (0, [])
        checkDup _ [] = (0, [])
        checkDup (x::xs) (y::ys) =
            if (x == card) then (y, 0 :: ys)
            else let (cc, uc) := checkDup xs ys in (cc, y :: uc)

matchType : List Nat -> HandType
matchType (5 :: _) = FiveOfAKind
matchType (4 :: 1 :: _) = FourOfAKind
matchType (3 :: 2 :: _) = FullHouse
matchType (3 :: 1 :: 1 :: _) = ThreeOfAKind
matchType (2 :: 2 :: 1 :: _) = TwoPair
matchType (2 :: 1 :: 1 :: 1 :: _) = OnePair
matchType _ = HighCard

cardStrength : Char -> Nat
cardStrength 'A' = 13
cardStrength 'K' = 12
cardStrength 'Q' = 11
cardStrength 'J' = 10
cardStrength 'T' = 9
cardStrength '9' = 8
cardStrength '8' = 7
cardStrength '7' = 6
cardStrength '6' = 5
cardStrength '5' = 4
cardStrength '4' = 3
cardStrength '3' = 2
cardStrength '2' = 1
cardStrength _   = 0

comparingCard : (Char -> Nat) -> List Char -> List Char -> Ordering
comparingCard strength [] [] = EQ
comparingCard strength [] (x :: xs) = EQ
comparingCard strength (x :: xs) [] = EQ
comparingCard strength (x :: xs) (y :: ys) =
    case comparing strength x y of
        EQ => comparingCard strength xs ys
        order => order

parseLine : String -> (List Char, Integer)
parseLine str =  bimap unpack (fromMaybe 0 . parseInteger) $ break isSpace str 


parseFile : (path : String) -> IO (List String)
parseFile path =
    let
        go : (List String) -> File -> IO (Either FileError (List String))
        go lines file = do
            False <- fEOF file | True => pure (Right lines)
            Right line <- fGetLine file
                | Left err => pure (Left err)
            if (strLength line > 0 ) then go (line :: lines) file else go lines file
    in do
        result <- withFile path Read pure (go [])
        case result of
            (Left err) => die "Got error when loading file"
            (Right lines) => pure lines

-- PART I


getType1 : List Char -> HandType
getType1 cards =
    (matchType . reverse . sort . count) cards 

compareHand1 : List Char -> List Char -> Ordering
compareHand1 h1 h2 =
    if (getType1 h1 == getType1 h2)
            then comparingCard cardStrength h1 h2
            else comparing getType1 h1 h2

calculate1 : List String -> Integer
calculate1 input =
    let 
        bids := map snd $ sortBy (compareHand1 `on` fst) $ parseLine <$> input
    in snd $ foldl (\acc, bid => (fst acc + 1, snd acc + (fst acc * bid))) (1, 0) bids


-- PART II

mutateCount : List Char -> List Nat -> (Nat, List Nat)
mutateCount [] [] = (0, [])
mutateCount [] (x :: xs) = (0, [])
mutateCount (x :: xs) [] = (0, [])
mutateCount (x :: xs) (y :: ys) =
    if (x == 'J') then (y, 0 :: ys)
    else let (jcount, counts) = mutateCount xs ys in (jcount, y :: counts)

getType2 : List Char -> HandType
getType2 cards =
    let (jcount, counts) := mutateCount cards $ count cards
        updatedCounts :=
            case (reverse . sort) counts of
                [] => [] 
                (x::xs) => (x+jcount) :: xs 
    in 
    matchType updatedCounts

cardStrength2 : Char -> Nat
cardStrength2 'J' = 0
cardStrength2 x   = cardStrength x

compareHand2 : List Char -> List Char -> Ordering
compareHand2 h1 h2 =
    if (getType2 h1 == getType2 h2)
            then comparingCard cardStrength2 h1 h2
            else comparing getType2 h1 h2

calculate2 : List String -> Integer
calculate2 input =
    let 
        bids := map snd $ sortBy (compareHand2 `on` fst) $ parseLine <$> input
    in snd $ foldl (\acc, bid => (fst acc + 1, snd acc + (fst acc * bid))) (1, 0) bids

-- Main

main : IO ()
main = do
    lines <- parseFile "src/day07/input.txt"
    printLn $ "Part I total winning: " ++ cast (calculate1 lines)
    printLn $ "Part I total winning: " ++ cast (calculate2 lines)
