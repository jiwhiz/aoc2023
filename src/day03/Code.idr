module Main

import Data.List
import Data.Maybe
import Data.String
import System.File


record NumData where
    constructor MkNumData
    line : Nat
    startCol : Nat
    endColPlus1 : Nat 
    value : Integer

implementation Show NumData where
    show v = (show v.value) ++ "(" ++ (show v.line) ++ ", " ++ (show v.startCol) ++ "-" ++ (show v.endColPlus1) ++ ")"


record SymbolData where
    constructor MkSymbolData
    line : Nat
    col : Nat
    value : Char

implementation Show SymbolData where
    show v = (show v.value) ++ "(" ++ (show v.line) ++ ", " ++ (show v.col) ++ ")"

adjacent : NumData -> SymbolData -> Bool
adjacent nd sd =
    (sd.line == nd.line && (S sd.col == nd.startCol || sd.col == nd.endColPlus1))
    || (S sd.line == nd.line && (S sd.col >= nd.startCol && sd.col <= nd.endColPlus1)) 
    || (sd.line == S nd.line && (S sd.col >= nd.startCol && sd.col <= nd.endColPlus1)) 

parseNum : List Char -> Integer
parseNum cl = fromMaybe 0 $ parseInteger $ pack cl

parseNumInLine : (l : Nat) -> String -> (List NumData, List SymbolData)
parseNumInLine l str =
    parse l 1 [] 0 (unpack str)
    where
        parse : (l : Nat) -> (c : Nat) -> (numChars : List Char) -> (nsCol : Nat) -> List Char -> (List NumData, List SymbolData)
        parse l c [] _ [] = ([], [])
        parse l c [] _ ['\n'] = ([], [])
        parse l c [] _ (x :: xs) =
            if isDigit x then parse l (S c) [x] c xs
            else if x == '.' then parse l (S c) [] 0 xs
            else -- find a symbol
                let (nl, sl) = parse l (S c) [] 0 xs
                in (nl, (MkSymbolData l c x) :: sl)
        parse l c nstr@(x :: xs) nsCol [] = ([(MkNumData l nsCol c (parseNum nstr))], [])
        parse l c nstr@(x :: xs) nsCol ['\n'] = ([(MkNumData l nsCol c (parseNum nstr))], [])
        parse l c nstr@(x :: xs) nsCol (y :: ys) =
            if isDigit y then parse l (S c) (nstr ++ [y]) nsCol ys
            else if y /= '.' then 
                let (nl, sl) = parse l (S c) [] 0 ys 
                in ((MkNumData l nsCol c (parseNum nstr)) :: nl, (MkSymbolData l c y) :: sl)
            else
                let (nl, sl) = parse l (S c) [] 0 ys 
                in ((MkNumData l nsCol c (parseNum nstr)) :: nl, sl)

parseFile : (path : String) -> IO (List NumData, List SymbolData)
parseFile path =
    let
        go : (l: Nat) -> (List NumData, List SymbolData) -> File -> IO (Either FileError (List NumData, List SymbolData))
        go l k file = do
            False <- fEOF file | True => pure (Right k)
            Right line <- fGetLine file
                | Left err => pure (Left err)
            let k' := (parseNumInLine l line)
            go (S l) (fst k ++ fst k', snd k ++ snd k') file
    in do
        result <- withFile path Read pure (go 1 ([], []))
        case result of
            (Left err) => pure ([], [])
            (Right n) => pure n

-- Part I

calculate : List NumData -> List SymbolData -> Integer 
calculate ndlst sdlst =
    foldl
        (\acc, nd =>
            case find (\sd => adjacent nd sd) sdlst of
                Nothing => acc
                Just _ => acc + nd.value
        )
        0
        ndlst

-- Part II

gearRatio : List NumData -> List SymbolData -> Integer 
gearRatio ndlst sdlst =
    foldl
        (\acc, sd =>
            case filter (\nd => adjacent nd sd) ndlst of
                fst :: snd :: [] => fst.value * snd.value + acc
                _ => acc
        )
        0
        sdlst

-- Main

main : IO ()
main = do
    (ndlst, sdlst) <- parseFile "src/day03/input.txt"
    printLn $ "Sum of part numbers: " ++ cast (calculate ndlst sdlst)
    printLn $ "Gear Ratio: " ++ cast (gearRatio ndlst sdlst)
