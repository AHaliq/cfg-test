module Lib
    ( someFunc
    ) where


import Prelude hiding (unlines)
import Text.Printf ( printf )
import Data.Tree
import StringUtil
import Cfg
import CfgAst

cfgq2 = fromRuleList "S" [
    "S ::= b A",
    "S ::= a B",
    "A ::= a",
    "A ::= a S",
    "A ::= b A A",
    "B ::= b",
    "B ::= b S",
    "B ::= a B B"]

cfgq2ans = fromRuleList "S" [
    "S ::= b A S'",
    "S ::= a B S'",
    "S' ::= S",
    "S' ::=",
    "A ::= a",
    "A ::= b A A",
    "B ::= b",
    "B ::= a B B"]

cfgq1 = fromRuleList "S" [
    "S ::= a S b",
    "S ::= a S a",
    "S ::= b S a",
    "S ::= b S b",
    "S ::="]

cfgq1ans = fromRuleList "S" [
    "S ::= a a S",
    "S ::= a b S",
    "S ::= b a S",
    "S ::= b b S",
    "S ::="]

h '0' = 'a'
h '1' = 'b'

sigma :: [String]
sigma = map (\x -> tail $ map h $ printf "%b" x) ([2..] :: [Int])


-- pure cfg property checkers
isValid cfg s = (numOfDerivations cfg s) >= 1
isAmbiguous cfg s = (numOfDerivations cfg s) > 1
numOfDerivations cfg s = length $ parse cfg s
getMostAmbiguous cfg range = foldl (\a@(as,ad) x@(s,d) -> if d > ad then x else a) ("",0) $ map (\s -> (s, numOfDerivations cfg s)) $ take range sigma

-- monadic cfg parser invokers
parseMG f (Just cfg) s = Just $ map f $ parse cfg s
parseMG _ Nothing _ = Nothing
parseM = parseMG id
parseMString = parseMG treeToString
parseMTree = parseMG drawTree
parseMLatex cfg s = parseMG (\t -> treeToLatex t s) cfg s

-- monadic cfg property checkers
maxDerivationCountM (Just cfg) range = Just $ foldl (max) 0 $ map (\s -> length $ parse cfg s) $ take range sigma
maxDerivationCountM Nothing _ = Nothing
checkAmbiguousM (Just c1) range = Just $ not $ all not $ map (isAmbiguous c1) $ take range sigma
checkAmbiguousM Nothing _ = Nothing

-- monadic cfg comparators
checkEquivM (Just c1) (Just c2) range = Just $ all (\s -> isValid c1 s== isValid c2 s) $ take range sigma
checkEquivM Nothing _ _ = Nothing
checkEquivM _ Nothing _ = Nothing

-- parser result printer
printListM (Just xs) = foldl (\a s -> putStrLn s >> a) (return ()) xs
printListM Nothing = putStrLn "nope"

someFunc :: IO ()
someFunc =
    case (cfgq2ans, cfgq2) of
        (Nothing,_) -> putStrLn "invalid cfg"
        (_,Nothing) -> putStrLn "invalid cfg"
        (Just cfg, Just cfgo) -> do
            s <- return $ head $ filter (isAmbiguous cfgo) sigma
            putStrLn "INPUT :"
            putStrLn s
            ds <- return $ parse cfg s
            putStrLn "---\n\nDERIVATIONS :"
            putStrLn $ unlines $ map treeToString ds
            putStrLn "---\n\nTREE :"
            printListM $ parseMTree cfgq2ans s
            putStrLn "--\n\nCHECK IF ANY FIRST 5000 LEXICOGRAPHICAL STRINGS IS AMBIGUOUS :"
            putStrLn $ maybe ("invalid cfg") show $ checkAmbiguousM cfgq2ans 5000
            putStrLn "--\n\nCHECK IF CFGS ARE EQUAL (BOTH ACCEPT AND REJECT FIRST 5000 STRINGS) : "
            putStrLn $ maybe ("invalid cfg") show $ checkEquivM cfgq2 cfgq2ans 5000