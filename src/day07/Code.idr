module Main

import Data.Fin
import Data.List
import Data.String
import Data.Vect
import System
import System.File

-- Util functions for sorting Vect
maxVect : {n : Nat} -> Vect (S n) Nat -> (Fin (S n), Nat)
maxVect {n = 0} (x :: xs) = (FZ, x)
maxVect {n = (S k)} (x :: xs) =
    let (idx, v) := maxVect xs 
    in if (x >= v) then (FZ, x) else (FS idx, v)

sortVect : {n : Nat} -> Vect (S n) Nat -> Vect (S n) Nat
sortVect {n = 0} xs = xs
sortVect {n = (S k)} xs =
    let (idx, max) := maxVect xs in max :: sortVect (deleteAt idx xs)

data HandType = FiveOfAKind | FourOfAKind | FullHouse | ThreeOfAKind | TwoPair | OnePair | HighCard

typeRank : HandType -> Nat
typeRank FiveOfAKind  = 6
typeRank FourOfAKind  = 5
typeRank FullHouse    = 4
typeRank ThreeOfAKind = 3
typeRank TwoPair      = 2
typeRank OnePair      = 1
typeRank HighCard     = 0

implementation Eq HandType where
    t1 == t2 = typeRank t1 == typeRank t2

implementation Ord HandType where
    compare = comparing typeRank

count : Vect n Char -> Vect n Nat
count [] = []
count (card :: remaining) = 
    let rCount := count remaining
        (cardCount, updatedCounts) := checkDup remaining rCount 
    in S cardCount :: updatedCounts
    where
        checkDup : Vect m Char -> Vect m Nat -> (Nat, Vect m Nat)
        checkDup [] [] = (0, [])
        checkDup (x::xs) (y::ys) =
            if (x == card) then (y, 0 :: ys)
            else let (cc, uc) := checkDup xs ys in (cc, y :: uc)

matchType : Vect 5 Nat -> HandType
matchType (5 :: _)                = FiveOfAKind
matchType (4 :: 1 :: _)           = FourOfAKind
matchType (3 :: 2 :: _)           = FullHouse
matchType (3 :: 1 :: 1 :: _)      = ThreeOfAKind
matchType (2 :: 2 :: 1 :: _)      = TwoPair
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

comparingCards : (Char -> Nat) -> Vect n Char -> Vect n Char -> Ordering
comparingCards strength [] [] = EQ
comparingCards strength (x :: xs) (y :: ys) =
    case comparing strength x y of
        EQ => comparingCards strength xs ys
        order => order

Hand : Type 
Hand = Vect 5 Char

parseHand : List Char -> Maybe Hand
parseHand cs = p 5 cs
    where
        p : (n: Nat) -> List Char -> Maybe (Vect n Char)
        p 0     [] = Just Nil
        p (S k) [] = Nothing
        p 0     (x :: xs) = Nothing
        p (S k) (x :: xs) =
            do 
                r <- p k xs
                pure (x :: r)

parseOneLine : String -> Maybe (Hand, Integer)
parseOneLine str =
    let
        (handStr, bidStr) := break isSpace str
    in do 
        hand <- parseHand $ unpack handStr
        bid <- parseInteger $ trim bidStr 
        pure (hand, bid)

-- PART I

partOne : Hand -> Hand -> Ordering
partOne h1 h2 =
    if getType h1 == getType h2
    then comparingCards cardStrength h1 h2
    else comparing getType h1 h2
    where
        getType : Hand -> HandType
        getType = matchType . sortVect . count

-- PART II

partTwo : Hand -> Hand -> Ordering
partTwo h1 h2 =
    if getType h1 == getType h2
    then comparingCards cardStrength2 h1 h2
    else comparing getType h1 h2
    where
        mutateCount : Vect n Char -> Vect n Nat -> (Nat, Vect n Nat)
        mutateCount [] [] = (0, [])
        mutateCount (x :: xs) (y :: ys) =
            if (x == 'J') then (y, 0 :: ys)
            else let (jcount, counts) = mutateCount xs ys in (jcount, y :: counts)

        getType : Hand -> HandType
        getType cards =
            let (jcount, counts) := mutateCount cards $ count cards
                (c::cs) := sortVect counts
            in matchType (c + jcount :: cs)

        cardStrength2 : Char -> Nat
        cardStrength2 'J' = 0
        cardStrength2 x   = cardStrength x

-- Main

calculate : (Hand -> Hand -> Ordering) -> List (Hand, Integer) -> Integer
calculate comp input =
    let 
        bids := map snd $ sortBy (comp `on` fst) input
    in snd $ foldl (\acc, bid => (fst acc + 1, snd acc + (fst acc * bid))) (1, 0) bids


doExercise : (Hand -> Hand -> Ordering) -> (path : String) -> (msg : String) -> IO ()
doExercise f path msg =
    let
        go : (List (Hand, Integer)) -> File -> IO (Either FileError (List (Hand, Integer)))
        go acc file = do
            False <- fEOF file | True => pure (Right acc)
            Right line <- fGetLine file
                | Left err => pure (Left err)
            case parseOneLine line of
                Nothing => go acc file
                Just v => go (v :: acc) file
    in do
        result <- withFile path Read pure (go [])
        case result of
            (Left err) => (printLn err)
            (Right n) => printLn $ msg ++ (cast $ calculate f n)

main : IO ()
main =
    do 
        doExercise partOne "src/day07/test.txt" "Part I test result: "
        doExercise partOne "src/day07/input.txt" "Part I result: "
        doExercise partTwo "src/day07/test.txt" "Part II test result: "
        doExercise partTwo "src/day07/input.txt" "Part II result: "
