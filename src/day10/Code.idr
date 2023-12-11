module Main

import Debug.Trace
import Data.Fin
import Data.List
import Data.String
import Data.Vect
import Decidable.Equality
import Util

data PipeType = H | V | F | L | J | Z
Show PipeType where
    show t = case t of
        H => "-"
        V => "|"
        F => "F"
        L => "L"
        J => "J"
        Z => "7"

data CellType = Space | Pipe PipeType | Start

Show CellType where
    show ct = case ct of
        Space => "Space"
        Start => "Start"
        Pipe t => "Pipe " ++ (show t)

parseCellType : Char -> CellType 
parseCellType '.' = Space 
parseCellType 'S' = Start
parseCellType '|' = Pipe V
parseCellType '-' = Pipe H
parseCellType 'F' = Pipe F
parseCellType 'L' = Pipe L
parseCellType 'J' = Pipe J
parseCellType '7' = Pipe Z
parseCellType _ = Space

record Cell where
    constructor MkCell
    c : Char
    t : CellType
    p : Bool

parseBoard : (m : Nat) -> (lines : List String) -> List (Vect m Cell)
parseBoard _ [] = []
parseBoard m (l :: ls) =
    let chars := unpack $ trim l in
    case decEq (length chars) m of
        (Yes Refl) => ((\c => MkCell c (parseCellType c) False) <$> fromList chars) :: parseBoard m ls
        (No _) => parseBoard m ls

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

findS : Vect n (Vect m Cell) -> Maybe (Fin n, Fin m)
findS [] = Nothing
findS (x :: xs) =
    case findIndex (\c => c.c == 'S') x of
        (Just col) => Just (FZ, col)
        Nothing => do
            (row, col) <- findS xs
            pure (FS row, col)


getCode : Vect n (Vect m Char) -> (Fin n, Fin m) -> Char
getCode matrix (row, col) =
    let
        line := index row matrix
        elem := index col line
    in elem

getCell : Vect n (Vect m Cell) -> (Fin n, Fin m) -> Cell
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

isConnectedPipe : Vect n (Vect m Cell) -> Dir -> (Fin n, Fin m) -> Bool
isConnectedPipe board dir pos =
    let (MkCell c t p) = getCell board pos
    in case c of
        '|' => (dir == Up || dir == Down)
        '-' => (dir == Right || dir == Left)
        'L' => (dir == Down || dir == Left)
        'J' => (dir == Down || dir == Right)
        '7' => (dir == Right || dir == Up)
        'F' => (dir == Left || dir == Up)
        _ => False

nextStep : {n, m : Nat} -> Vect n (Vect m Cell) -> (Fin n, Fin m) -> Dir -> Maybe (Dir, (Fin n, Fin m))
nextStep board pos dir =
    let (MkCell c t p) := getCell board pos
    in case (dir, c) of
        (_, 'S') => Nothing
        (Right, '-') => goRight pos
        (Left, '-') => goLeft pos
        (Up, '|') => goUp pos
        (Down, '|') => goDown pos
        (Down, 'L') => goRight pos
        (Left, 'L') => goUp pos
        (Down, 'J') => goLeft pos
        (Right, 'J') => goUp pos
        (Right, '7') => goDown pos
        (Up, '7') => goLeft pos
        (Up, 'F') => goRight pos
        (Left, 'F') => goDown pos
        _ => Nothing 

move : {n, m : Nat} -> (Fin n, Fin m) -> Dir -> Maybe (Dir, (Fin n, Fin m))
move pos Up = goUp pos
move pos Down = goDown pos
move pos Left = goLeft pos
move pos Right = goRight pos

startS : {n, m : Nat} -> Vect n (Vect m Cell) -> (Fin n, Fin m) -> Dir -> Maybe (Dir, (Fin n, Fin m))
startS board posS dir =
    do
        (nextDir, nextPos) <- move posS dir
        if (isConnectedPipe board dir nextPos) then Just (nextDir, nextPos) else Nothing

markPath : {n, m : Nat} -> Vect n (Vect m Cell) -> (Fin n, Fin m) -> Vect n (Vect m Cell)
markPath matrix (row, col) = 
    let updatedRow := updateAt col (\cell => {p:=True} cell ) $ index row matrix
    in replaceAt row updatedRow matrix

setStartPipe : Dir -> Dir -> (Fin n, Fin m) -> Vect n (Vect m Cell) -> Vect n (Vect m Cell)
setStartPipe startDir endDir (row, col) matrix =
    let pipe := case (startDir, endDir) of
            (Up, Up) => Pipe V
            (Up, Left) => Pipe L 
            (Up, Right) => Pipe J
            (Down, Down) => Pipe V
            (Down, Left) => Pipe F
            (Down, Right) => Pipe Z
            (Right, Right) => Pipe H
            (Right, Up) => Pipe F
            (Right, Down) => Pipe L
            (Left, Left) => Pipe H
            (Left, Up) => Pipe Z
            (Left, Down) => Pipe J
            _ => Pipe H -- should not happen
        
        updatedRow := updateAt col (\cell => {p:=True, t:=pipe} cell ) $ index row matrix
    in 
        replaceAt row updatedRow matrix


cal : {n, m : Nat} -> Vect n (Vect m Cell) -> Maybe (Nat, Vect n (Vect m Cell))
cal board =
    do
        posS <- findS board
        let tryl := (startS board posS) <$> [Up,Right,Down,Left]
        r <- Data.Vect.find isJust tryl
        (dir, pos) <- r
        walk dir board dir pos 1
    where
        walk : Dir -> Vect n (Vect m Cell) -> Dir -> (Fin n, Fin m) -> Nat -> Maybe (Nat, Vect n (Vect m Cell))
        walk startDir board dir pos steps =
            let newBoard := markPath board pos
                (MkCell c t p) := getCell board pos
            in
                if c == 'S' then Just (steps, setStartPipe startDir dir pos newBoard)
                else 
                    case nextStep board pos dir of
                        Nothing => Nothing
                        Just (nextDir, nextPos) => walk startDir newBoard nextDir nextPos (S steps)

count : Vect _ Cell -> (Bool, Maybe PipeType) -> Nat
count [] _ = 0
count (cell :: xs) (turnOn, preCell) =
    if (cell.p == True) then
        case cell.t of
            Pipe H => count xs (turnOn, preCell)
            Pipe L => count xs (turnOn, Just L)
            Pipe J => count xs (case preCell of Just F => (not turnOn, Nothing); _ => (turnOn, Nothing))
            Pipe Z => count xs (case preCell of Just L => (not turnOn, Nothing); _ => (turnOn, Nothing))
            Pipe F => count xs (turnOn, Just F)
            _ => count xs (not turnOn, Nothing)
    else
        if turnOn then S (count xs (turnOn, Nothing)) else count xs (turnOn, Nothing)

countTile : Vect n (Vect m Cell) -> Nat
countTile [] = 0
countTile (x :: xs) = count x (False, Nothing) + countTile xs

printBoard : Vect n (Vect m Cell) -> IO ()
printBoard [] = do pure ()
printBoard (x :: xs) = do
    printRow x
    printBoard xs
    where   
        printRow : Vect k Cell -> IO ()
        printRow row = do 
            printLn $ pack $ toList $
                map
                    (\c=>
                        if c.c == 'S' then 'S' else
                            if c.p then '#' else '.')
                    row

covering
main : IO ()
main =
    do 
        (l :: lines) <- Util.parseFile "src/day10/input.txt" | [] => printLn "Error parse file"

        let board := fromList $ parseBoard (length $ trim l) (l::lines)
            Just (steps, newBoard) := cal board
            | Nothing => printLn "Error calculate steps"
        
        -- printBoard newBoard
        printLn $ "Part I result: " ++ (show $ div steps 2)
        printLn $ "Part II result: " ++ (show $ countTile newBoard)
