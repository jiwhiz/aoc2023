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
parseCellType '.'  = S
parseCellType '/'  = F
parseCellType '\\' = B
parseCellType '-'  = H
parseCellType '|'  = V
parseCellType  _   = S


record Cell where
    constructor MkCell
    t : CellType
    energized : Vect 4 Bool

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

forwardMirror : Dir -> Dir
forwardMirror = \case {Up=>Right; Down=>Left; Left=>Down; Right=>Up}

backwardMirror : Dir -> Dir
backwardMirror = \case {Up=>Left; Down=>Right; Left=>Up; Right=>Down}

vertSplitter1 : Dir -> Dir
vertSplitter1 = \case {Up=>Up; Down=>Down; Left=>Up; Right=>Up}

vertSplitter2 : Dir -> Dir
vertSplitter2 = \case {Up=>Up; Down=>Down; Left=>Down; Right=>Down}

horizSplitter1 : Dir -> Dir
horizSplitter1 = \case {Up=>Right; Down=>Right; Left=>Left; Right=>Right}

horizSplitter2 : Dir -> Dir
horizSplitter2 = \case {Up=>Left; Down=>Left; Left=>Left; Right=>Right}

nextSteps : {n, m : Nat} -> CellType -> (Fin n, Fin m) -> Dir -> List (Maybe (Dir, (Fin n, Fin m)))
nextSteps t pos dir =
    case t of
        S => [next pos dir]
        F => [next pos $ forwardMirror dir]
        B => [next pos $ backwardMirror dir]
        V => [next pos $ vertSplitter1 dir, next pos $ vertSplitter2 dir]
        H => [next pos $ horizSplitter1 dir, next pos $ horizSplitter2 dir]
    where
        next : (Fin n, Fin m) -> Dir -> Maybe (Dir, (Fin n, Fin m))
        next (row, col) dir =
            case dir of
                Up => case row of FZ => Nothing; FS x => Just (Up, (weaken x, col))
                Down => do newRow <- strengthen (FS row); pure (Down, (newRow, col))
                Left => case col of FZ => Nothing; FS y => Just (Left, (row, weaken y))
                Right => do newCol <- strengthen (FS col); pure (Right, (row, newCol))

enterBefore : Vect 4 Bool -> Dir -> Bool 
enterBefore [u, d, l, r] = \case {Up => u; Down => d; Left => l; Right => r}

energize : Cell -> Dir -> Cell
energize (MkCell t [u, d, l, r]) = \case {Up => MkCell t [True, d, l, r]; Down => MkCell t [u, True, l, r]; Left => MkCell t [u, d, True, r]; Right => MkCell t [u, d, l, True]}

process : {n, m : Nat} -> Board n m -> (Fin n, Fin m) -> Dir -> Board n m
process board pos@(row, col) dir = 
    let cell@(MkCell t e) := getCell board pos
    in
        if (enterBefore e dir) then board
        else foldl
            (\b, step => case step of 
                Nothing => b 
                Just (nextDir, nextPos) => 
                    process b nextPos nextDir
            )
            (replaceAt row (replaceAt col (energize cell dir) $ index row board) board)
            (nextSteps t pos dir )

calculate : {n, m : Nat} -> Board n m -> Nat 
calculate [] = 0
calculate (row :: rows) = cal row + calculate rows
    where
        cal : Vect l Cell -> Nat
        cal [] = 0
        cal (MkCell _ [u, d, l, r] ::xs) = if u || d || r || l then S $ cal xs else cal xs

parseBoard : (m : Nat) -> (lines : List String) -> List (Vect m Cell)
parseBoard _ [] = []
parseBoard m (l :: ls) =
    let chars := unpack $ trim l in
    case decEq (length chars) m of
        (Yes Refl) => ((\c => MkCell (parseCellType c) [False, False, False, False]) <$> fromList chars) :: parseBoard m ls
        (No _) => parseBoard m ls

printBoard : {n, m : Nat} -> Board n m -> IO ()
printBoard [] = do pure ()
printBoard (x :: xs) = do
    printRow x
    printBoard xs
    where
        showFlow : Vect 4 Bool -> String
        showFlow flow =
            case sum $ (\b => if b then 1 else 0) <$> flow of
                0 => "."
                1 => foldl (\acc, (b,c) => acc ++ if b then c else "") "" (zip flow ["^", "v", "<", ">"])
                x => show x

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

        printLn $ "Part I result: " ++ (show $ calculate $ process initBoard (FZ, FZ) Right)

        let left := foldl (\acc, f => (calculate $ process initBoard (f, FZ) Right) :: acc) (the (List Nat) []) (allFins (length ps))
        let right := foldl (\acc, f => (calculate $ process initBoard (f, last) Left) :: acc) (the (List Nat) []) (allFins (length ps))
        let top := foldl (\acc, f => (calculate $ process initBoard (FZ, f) Down) :: acc) (the (List Nat) []) (allFins cols)
        let bottom := foldl (\acc, f => (calculate $ process initBoard (last, f) Up) :: acc) (the (List Nat) []) (allFins cols)
        printLn $ "Part II result: " ++ (show $ foldl (\acc, e => if e > acc then e else acc) 0 (left ++ right ++ top ++ bottom))
        pure ()
