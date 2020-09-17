module Lib
    ( someFunc
    ) where


import Prelude hiding (unlines)
import Text.Printf ( printf )

import StringUtil
import Cfg
import CfgAst

cfg1 = fromRuleList "S" [
    "S ::= b A",
    "S ::= a B",
    "A ::= a",
    "A ::= a S",
    "A ::= b A A",
    "B ::= b",
    "B ::= b S",
    "B ::= a B B"]

h '0' = 'a'
h '1' = 'b'

sigma :: [String]
sigma = map (\x -> tail $ map h $ printf "%b" x) ([2..] :: [Int])


isAmbiguous cfg s = (numOfDerivations cfg s) > 1
numOfDerivations cfg s = length $ parse cfg s
getMostAmbiguous cfg range = foldl (\a@(as,ad) x@(s,d) -> if d > ad then x else a) ("",0) $ map (\s -> (s, numOfDerivations cfg s)) $ take range sigma

someFunc :: IO ()
someFunc =
    case cfg1 of
        Nothing -> putStrLn "invalid cfg"
        Just cfg -> do
            s <- return $ head $ filter (isAmbiguous cfg) sigma
            putStrLn "INPUT :"
            putStrLn s
            ds <- return $ map fst $ parse cfg s
            putStrLn "---\n\nDERIVATIONS :"
            putStrLn $ unlines $ map treeToString ds
            putStrLn "---\n\nLATEX TREE :"
            putStrLn $ unlines $ map (\(i,t) -> treeToLatex t (unwords [s, "derivation", show i])) $ zip [1..(length ds)] ds