module Main

import Data.Fin
import Data.Fuel
import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.Vect
import System
import System.File
import Data.SortedMap
import Util

%default total

data Instr = LEFT | RIGHT

Show Instr where
    show i = case i of 
        LEFT => "L"
        RIGHT => "R"


parseInstrs : String -> List Instr
parseInstrs str = reverse $ parse (unpack str) []
    where
        parse : List Char -> List Instr -> List Instr
        parse [] acc = acc
        parse (c :: cs) acc =
            case c of
                'L' => parse cs (LEFT :: acc)
                'R' => parse cs (RIGHT :: acc)
                _   => parse cs acc

Node : Type
Node = Vect 3 Char

parseMap : List String -> SortedMap Node (Node, Node)
parseMap = doParse SortedMap.empty
    where 
        doParse : SortedMap Node (Node, Node) -> List String -> SortedMap Node (Node, Node)
        doParse networkMap [] = networkMap
        doParse networkMap (l::lines) =
            let chars := unpack l
            in
                case chars of
                    (n1 :: n2 :: n3 :: ' ' :: '=' :: ' ' :: '(' ::
                        l1 :: l2 :: l3 :: ',' :: ' ' :: r1 :: r2 :: r3 :: ')' :: _) =>
                        doParse (SortedMap.insert [n1, n2, n3] ([l1, l2, l3], [r1, r2, r3]) networkMap) lines
                    _ => doParse networkMap lines

||| In the network map, take one step based on instruction from start node, return Nothing if cannot find start node
oneStep : SortedMap Node (Node, Node) -> Instr -> Node -> Maybe Node
oneStep networkMap instr start = do
    (left, right) <- lookup start networkMap
    case instr of
        LEFT => pure left
        RIGHT => pure right

-- PART I

run1 : Fuel -> List Instr -> SortedMap Node (Node, Node) -> Node -> Nat -> Nat
run1 (More fuel) instrs networkMap start preSteps =
    let
        Just (steps, node) := navigate instrs start | Nothing => 0
    in
        if (node == ['Z', 'Z', 'Z']) then preSteps + steps
        else run1 fuel instrs networkMap node (preSteps + steps)
    where
        navigate : List Instr -> Node -> Maybe (Nat, Node)
        navigate [] start = Just (0, start) -- reached the end of instructions
        navigate (x :: xs) start = do 
            ['Z', 'Z', 'Z'] <- oneStep networkMap x start
                | nextNode => do
                    (steps, endNode) <- navigate xs nextNode
                    pure (S steps, endNode)
            pure (1, ['Z', 'Z', 'Z'])
run1 Dry _ _ _ _ = 0

-- PART II

||| find nodes with specific ending letter
parseStartingNodes : Char -> List String -> List Node
parseStartingNodes endingChar = doParse [] where
    doParse : List Node -> List String -> List Node
    doParse acc [] = acc
    doParse acc (x :: xs) =
        case unpack x of
            a :: b :: c :: _ => if (c == endingChar) then doParse ([a, b, c] :: acc) xs else doParse acc xs
            _ => doParse acc xs

||| Map for a starting node (**A or **Z) to ending node (**Z) with steps
calculateRoundMap : List Instr -> SortedMap Node (Node, Node) -> List Node -> Maybe (SortedMap Node (Nat, Node))
calculateRoundMap instrs networkMap startNodes =
    fromList <$> zip startNodes <$> traverse (naviWithFuel (limit 10000)) ((\x => (0, x)) <$> startNodes)
    where
        ||| navi one node to get steps and final node ended with Z
        naviWithFuel : Fuel -> (Nat, Node) -> Maybe (Nat, Node)
        naviWithFuel Dry _ = Nothing
        naviWithFuel (More fuel) (previousSteps, startNode) =
            case doNavi instrs startNode of
                Nothing => Nothing
                Just (steps, node@[_, _, 'Z']) => Just (steps + previousSteps, node)
                Just (steps, node) => naviWithFuel fuel (steps + previousSteps, node)
            where
                doNavi : List Instr -> Node -> Maybe (Nat, Node)
                doNavi [] start = Just (0, start) -- reach end of instr
                doNavi (instr :: is) start = 
                    case oneStep networkMap instr start of
                            Nothing => Nothing  -- stuck
                            Just z@[_, _, 'Z'] => Just (1, z)  -- found ending node
                            Just next => do
                                (s, end) <- doNavi is next
                                pure (S s, end)

sameSteps : List1 (Nat, Node) -> Bool
sameSteps ((steps, _) ::: nodes) = foldl (\acc, n => acc && steps == fst n) True nodes

maxSteps : List1 (Nat, Node) -> Nat
maxSteps (head ::: tail) = foldl (\acc, n => max acc (fst n)) (fst head) tail

covering
newRound : SortedMap Node (Nat, Node) -> List1 (Nat, Node) -> List1 (Nat, Node)
newRound roundMap nodes@(head ::: tail) = 
    let
        curMax := maxSteps nodes
    in
        (stepForward head curMax) ::: doNewRound curMax tail
    where 
        covering
        stepForward : (Nat, Node) -> Nat -> (Nat, Node)
        stepForward (curSteps, node) curMax =
            if curSteps < curMax then
                case lookup node roundMap of
                    Nothing => (curSteps, node) -- ??
                    Just (steps, nextNode) => stepForward (curSteps + steps, nextNode) curMax
            else (curSteps, node)

        covering
        doNewRound : Nat -> List (Nat, Node) -> List (Nat, Node)
        doNewRound _ [] = []
        doNewRound curMax (x :: xs) = stepForward x curMax :: doNewRound curMax xs

covering
run2 : Fuel -> List Instr -> SortedMap Node (Node, Node) -> SortedMap Node (Nat, Node) -> List1 (Nat, Node) -> Nat
run2 (More fuel) instrs networkMap roundMap nodesWithSteps =
    if sameSteps nodesWithSteps then (fst . head) nodesWithSteps
    else run2 fuel instrs networkMap roundMap (newRound roundMap nodesWithSteps)
run2 Dry _ _ _ _ = 0


-- -- Main
covering
main : IO ()
main =
    do 
        lines <- Util.parseFile "src/Day08/input.txt"
        case lines of
            (instrLine :: _ :: mapLines) => 
                let map := parseMap mapLines
                    instrs := parseInstrs instrLine
                    aNodes := parseStartingNodes 'A' mapLines
                    zNodes := parseStartingNodes 'Z' mapLines
                    (Just rMap) := calculateRoundMap instrs map (aNodes ++ zNodes) | Nothing => printLn "Error calculating round map"
                    Just (shead::stail) := traverse (\n => lookup n rMap) aNodes | _ => printLn "Error calculating starting nodes"
                in do
                    printLn $ "Part I Result is " ++ (cast $ run1 (limit 100) instrs map ['A', 'A', 'A'] 0)
                    printLn $ "Part II Result is " ++ (cast $ run2 (limit 10000000000) instrs map rMap (shead ::: stail))
            _ => printLn "Error input data"
