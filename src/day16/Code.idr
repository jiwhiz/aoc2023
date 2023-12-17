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

data CellType = S | F | B | H | V

Eq CellType where
    S == S = True
    F == F = True
    B == B = True
    H == H = True
    V == V = True
    _ == _ = False

Show CellType where
    show ct = case ct of
        S => "."
        F => "/"
        B => "`"
        H => "-"
        V => "|"

parseCellType : Char -> CellType 
parseCellType '.' = S
parseCellType '/' = F
parseCellType '\\' = B
parseCellType '-' = H
parseCellType '|' = V
parseCellType  _  = S


record Cell where
    constructor MkCell
    t : CellType
    energized : (Bool, Bool, Bool, Bool)


data Dir = Up | Down | Left | Right

Eq Dir where
    Up == Up = True
    Down == Down = True
    Left == Left = True
    Right == Right = True
    _ == _ = False

Show Dir where
    show d =
        case d of 
            Up => "U"
            Down => "D"
            Left => "L"
            Right => "R"

Board : Nat -> Nat -> Type
Board n m = Vect n (Vect m Cell)

getCell : {n, m : Nat} -> Board n m -> (Fin n, Fin m) -> Cell
getCell matrix (row, col) =
    let
        line := index row matrix
        elem := index col line
    in elem

goUp : (Fin n, Fin m) -> Maybe (Dir, (Fin n, Fin m))
goUp (FZ, _) = Nothing
goUp ((FS x), y) = Just (Up, (weaken x, y))

goDown : {n:Nat} -> (Fin n, Fin m) -> Maybe (Dir, (Fin n, Fin m))
goDown (x, y) =
    case strengthen (FS x) of 
        Nothing => Nothing
        Just sx => Just (Down, (sx, y))

goLeft : (Fin n, Fin m) -> Maybe (Dir, (Fin n, Fin m))
goLeft (_, FZ) = Nothing
goLeft (x, (FS y)) = Just (Left, (x, weaken y))

goRight : {m:Nat} -> (Fin n, Fin m) -> Maybe (Dir, (Fin n, Fin m))
goRight (x, y) =
    case strengthen (FS y) of 
        Nothing => Nothing
        Just sy => Just (Right, (x, sy))

nextSteps : {n, m : Nat} -> CellType -> (Fin n, Fin m) -> Dir -> List (Maybe (Dir, (Fin n, Fin m)))
nextSteps t pos dir =
    case (dir, t) of
        (Right, S) => [goRight pos]
        (Left,  S) => [goLeft pos]
        (Up,    S) => [goUp pos]
        (Down,  S) => [goDown pos]
        (Right, F) => [goUp pos]
        (Left,  F) => [goDown pos]
        (Up,    F) => [goRight pos]
        (Down,  F) => [goLeft pos]
        (Right, B) => [goDown pos]
        (Left,  B) => [goUp pos]
        (Up,    B) => [goLeft pos]
        (Down,  B) => [goRight pos]
        (Right, V) => [goUp pos, goDown pos]
        (Left,  V) => [goUp pos, goDown pos]
        (Up,    V) => [goUp pos]
        (Down,  V) => [goDown pos]
        (Right, H) => [goRight pos]
        (Left,  H) => [goLeft pos]
        (Up,    H) => [goRight pos, goLeft pos]
        (Down,  H) => [goRight pos, goLeft pos]

isEnergized : Dir -> (Bool, Bool, Bool, Bool) -> Bool 
isEnergized dir (u, d, l, r) =
    case dir of
        Up => u
        Down => d
        Left => l
        Right => r

energize : Cell -> Dir -> Cell
energize (MkCell t (u, d, l, r)) dir =
    case dir of
        Up => MkCell t (True, d, l, r)
        Down => MkCell t (u, True, l, r)
        Left => MkCell t (u, d, True, r)
        Right => MkCell t (u, d, l, True)

process : {n, m : Nat} -> Board n m -> (Fin n, Fin m) -> Dir -> Board n m
process board pos@(row, col) dir = 
    let cell@(MkCell t e) := getCell board pos
    in
        if isEnergized dir e then board
        else foldl 
            (\b, step => case step of 
                Nothing => b 
                Just (nextDir, nextPos) => 
                    process b nextPos nextDir
            )
            (replaceAt row (replaceAt col (energize cell dir) $ index row board) board)
            (nextSteps t pos dir )


cal1 : {n, m : Nat} -> Board n m -> Nat 
cal1 [] = 0
cal1 (row :: rows) = cal row + cal1 rows
    where
        cal : Vect l Cell -> Nat
        cal [] = 0
        cal (MkCell _ (u, d, l, r) ::xs) = if u || d || r || l then S $ cal xs else cal xs

parseBoard : (m : Nat) -> (lines : List String) -> List (Vect m Cell)
parseBoard _ [] = []
parseBoard m (l :: ls) =
    let chars := unpack $ trim l in
    case decEq (length chars) m of
        (Yes Refl) => ((\c => MkCell (parseCellType c) (False, False, False, False)) <$> fromList chars) :: parseBoard m ls
        (No _) => parseBoard m ls

showFlow : (Bool, Bool, Bool, Bool) -> String
showFlow (False, False, False, False) = "."
showFlow (False, False, False, True)  = ">"
showFlow (False, False, True,  False) = "<"
showFlow (False, False, True,  True)  = "2"
showFlow (False, True,  False, False) = "v"
showFlow (False, True,  False, True)  = "2"
showFlow (False, True,  True,  False) = "2"
showFlow (False, True,  True,  True)  = "3"
showFlow (True,  False, False, False) = "^"
showFlow (True,  False, False, True)  = "2"
showFlow (True,  False, True,  False) = "2"
showFlow (True,  False, True,  True)  = "3"
showFlow (True,  True,  False, False) = "2"
showFlow (True,  True,  False, True)  = "3"
showFlow (True,  True,  True,  False) = "3"
showFlow (True,  True,  True,  True)  = "4"

printBoard : {n, m : Nat} -> Board n m -> IO ()
printBoard [] = do pure ()
printBoard (x :: xs) = do
    printRow x
    printBoard xs
    where   
        printRow : Vect k Cell -> IO ()
        printRow row = do 
            printLn $ joinBy "" $ toList $
                map
                    (\(MkCell t e)=>
                        if t == S then showFlow e else show t)
                    row

covering
main : IO ()
main =
    do
        (l :: lines) <- Util.parseFile "src/day16/input.txt" | [] => printLn "Error parse file"
        let (S cols) := length l | Z => printLn "No data"
            (p::ps) := parseBoard (S cols) (l::lines) | [] => printLn "Error parse file"
            initBoard := fromList (p::ps)

        printLn $ "Part I result: " ++ (show $ cal1 $ process initBoard (FZ, FZ) Right)

        let left := foldl (\acc, f => (cal1 $ process initBoard (f, FZ) Right) :: acc) (the (List Nat) []) (allFins (length ps))
        let right := foldl (\acc, f => (cal1 $ process initBoard (f, last) Left) :: acc) (the (List Nat) []) (allFins (length ps))
        let top := foldl (\acc, f => (cal1 $ process initBoard (FZ, f) Down) :: acc) (the (List Nat) []) (allFins cols)
        let bottom := foldl (\acc, f => (cal1 $ process initBoard (last, f) Up) :: acc) (the (List Nat) []) (allFins cols)
        printLn $ "Part II result: " ++ (show $ foldl (\acc, e => if e > acc then e else acc) 0 (left ++ right ++ top ++ bottom))
        pure ()
