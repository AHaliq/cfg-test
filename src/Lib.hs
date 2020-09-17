module Lib
    ( someFunc
    ) where

import Text.Printf
import Prelude hiding (unlines)

indent s = unlines $ map (\s -> unwords ["   ", s]) $ lines s
unlines [] = []
unlines ([] : []) = []
unlines ([] : ss) = '\n' : unlines ss
unlines ((x : xs) : ss) = x : unlines (xs : ss)

data S = S1 A | S2 B            deriving (Show, Eq)
data A = A1 | A2 S | A3 A A     deriving (Show, Eq)
data B = B1 | B2 S | B3 B B     deriving (Show, Eq)

class LatexTree a where
    node :: a -> String
    children :: a -> [String]
    showL :: a -> String
    showL x = unlines $ ("[.\\textit{"++node x++"}"):(map indent $ children x)++["]"]

lA = "\\textbf{a}"
lB = "\\textbf{b}"

latexReady a caption = unlines ["\\begin{figure}[h]", "\\centering", "\\begin{tikzpicture}[level 1/.style={level distance=1.5cm}]", "\\Tree", showL a, "\\end{tikzpicture}", "\\caption{" ++ caption ++ "}", "\\label{fig:my_label}", "\\end{figure}"]

instance LatexTree S where
    node _ = "S"
    children (S1 a) = [lB, showL a]
    children (S2 b) = [lA, showL b]

instance LatexTree A where
    node _ = "A"
    children (A1) = [lA]
    children (A2 s) = [lA, showL s]
    children (A3 a1 a2) = [lB, showL a1, showL a2]

instance LatexTree B where
    node _ = "B"
    children (B1) = [lB]
    children (B2 s) = [lB, showL s]
    children (B3 b1 b2) = [lA, showL b1, showL b2]

class ToString a where
    toString :: a -> String

instance ToString S where
    toString (S1 a) = 'b':(toString a)
    toString (S2 b) = 'a':(toString b)

instance ToString A where
    toString (A1) = "a"
    toString (A2 s) = 'a':(toString s)
    toString (A3 a1 a2) = 'b':((toString a1) ++ (toString a2))

instance ToString B where
    toString (B1) = "b"
    toString (B2 s) = 'b':(toString s)
    toString (B3 b1 b2) = 'a':((toString b1) ++ (toString b2))

parseS :: String -> [(S,String)]
parseS ('b':s) = map (\(a,r) -> (S1 a, r)) $ parseA s
parseS ('a':s) = map (\(b,r) -> (S2 b, r)) $ parseB s
parseS _ = []

parseA :: String -> [(A,String)]
parseA ('a':s) = (A1, s): (map (\(s,r) -> (A2 s, r)) $ parseS s)
parseA ('b':s) = concatMap (\(a1, r) -> map (\(a2, r) -> (A3 a1 a2, r)) $ parseA r) $ parseA s
parseA _ = []

parseB :: String -> [(B,String)]
parseB ('b':s) = (B1, s):(map (\(s,r) -> (B2 s, r)) $ parseS s)
parseB ('a':s) = concatMap (\(b1, r) -> map (\(b2, r) -> (B3 b1 b2, r)) $ parseB r) $ parseB s
parseB _ = []

h '0' = 'a'
h '1' = 'b'

sigma :: [String]
sigma = map (\x -> tail $ map h $ printf "%b" x) ([2..] :: [Int])


validParseS s = filter (\(_, r) -> length r == 0) $ parseS s
isAmbiguous s = (numOfDerivations s) > 1
numOfDerivations s = length $ validParseS s
getMostAmbiguous range = foldl (\a@(as,ad) x@(s,d) -> if d > ad then x else a) ("",0) $ map (\s -> (s, numOfDerivations s)) $ take range sigma
printTrees s = unlines $ map (\(i,(t,_)) -> latexReady t (unwords [s, "derivation", show i])) $ zip ix ds
    where
        ds = parseS s
        ix = [1..(length ds)]

someFunc :: IO ()
someFunc = do
    s <- return $ head $ filter isAmbiguous sigma
    putStrLn "INPUT :"
    putStrLn s
    ds <- return $ map fst $ parseS s
    putStrLn "---\n\nDERIVATIONS :"
    putStrLn $ show $ ds
    putStrLn "---\n\nLATEX TREE :"
    putStrLn $ printTrees s
