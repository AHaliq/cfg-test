module Lib
    ( someFunc
    ) where


import Prelude hiding (unlines)
import Text.Printf ( printf )
import Data.Tree
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

cfg2 = fromRuleList "S" [
    "S ::= b A",
    "S ::= a B",
    "A ::= a",
    "A ::= a S",
    "A ::= b A a",
    "A ::= b A a S",
    "B ::= b",
    "B ::= b S",
    "B ::= a B B"]

cfgs1 = fromRuleList "S" [
    "S ::= a S b",
    "S ::= a S a",
    "S ::= b S a",
    "S ::= b S b",
    "S ::="]

cfgs2 = fromRuleList "S" [
    "S ::= a a S",
    "S ::= a b S",
    "S ::= b a S",
    "S ::= b b S",
    "S ::="]

h '0' = 'a'
h '1' = 'b'

sigma :: [String]
sigma = map (\x -> tail $ map h $ printf "%b" x) ([2..] :: [Int])


isValid cfg s = (numOfDerivations cfg s) >= 1
isAmbiguous cfg s = (numOfDerivations cfg s) > 1
numOfDerivations cfg s = length $ parse cfg s
getMostAmbiguous cfg range = foldl (\a@(as,ad) x@(s,d) -> if d > ad then x else a) ("",0) $ map (\s -> (s, numOfDerivations cfg s)) $ take range sigma

parseMG f (Just cfg) s = Just $ map f $ parse cfg s
parseMG _ Nothing _ = Nothing

parseM = parseMG id
parseMString = parseMG treeToString
parseMTree = parseMG drawTree

maxDerivationCountM (Just cfg) range = Just $ foldl (max) 0 $ map (\s -> length $ parse cfg s) $ take range sigma
maxDerivationCountM Nothing _ = Nothing

checkAmbiguousM (Just c1) range = Just $ not $ all not $ map (isAmbiguous c1) $ take range sigma
checkAmbiguousM Nothing _ = Nothing

checkEquivM (Just c1) (Just c2) range = Just $ all (\s -> isValid c1 s== isValid c2 s) $ take range sigma
checkEquivM Nothing _ _ = Nothing
checkEquivM _ Nothing _ = Nothing

printListM (Just xs) = foldl (\a s -> putStrLn s >> a) (return ()) xs
printListM Nothing = putStrLn "nope"

someFunc :: IO ()
someFunc =
    case cfg1 of
        Nothing -> putStrLn "invalid cfg"
        Just cfg -> do
            s <- return $ head $ filter (isAmbiguous cfg) sigma
            putStrLn "INPUT :"
            putStrLn s
            ds <- return $ parse cfg s
            putStrLn "---\n\nDERIVATIONS :"
            putStrLn $ unlines $ map treeToString ds
            putStrLn "---\n\nLATEX TREE :"
            putStrLn $ unlines $ map (\(i,t) -> treeToLatex t (unwords [s, "derivation", show i])) $ zip [1..(length ds)] ds