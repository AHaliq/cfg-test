module CfgAst 
  ( parse
  , treeToString
  , treeToLatex
  )where

import Prelude hiding (unlines)
import Cfg
import Data.Tree
import StringUtil

type Symbols = [String]
type AST = Tree String

parse cfg i = let s0 = getStart cfg in pruneValid $ matchRules i (getRules s0 cfg) (leaf s0)
  where
    matchRules :: String -> [Symbols] -> AST -> [(AST, String)]
    matchRules i rs n = concat $ foldl (\acc ts -> case matchRule i ts n of xxs@(x:xs) -> xxs:acc; [] -> acc) [] rs
    matchRule :: String -> Symbols -> AST -> [(AST,String)]
    matchRule i (t:ts) n
      | isTerminal t = case gobble i t of Just i -> matchRule i ts (addChild n $ leaf t); Nothing -> []
      | otherwise = concat $ map (\(n',i) -> matchRule i ts $ addChild n n') $ matchRules i (getRules t cfg) (leaf t)
    matchRule i [] n = [(n,i)]

pruneValid xs = filter (\(_,s) -> length s == 0) xs

addChild Node{rootLabel=a,subForest=xs} x = Node {rootLabel=a,subForest=x:xs}
leaf x = Node{rootLabel=x,subForest=[]}

treeToString Node{rootLabel=a,subForest=xs@(_:_)} = '(': unwords [a, (unwords $ map treeToString $ reverse xs)] ++ ")"
treeToString Node{rootLabel=a,subForest=[]} = a

treeToLatex t caption = unlines ["\\begin{figure}[h]", "\\centering", "\\begin{tikzpicture}[level 1/.style={level distance=1.5cm}]", "\\Tree", aux t, "\\end{tikzpicture}", "\\caption{" ++ caption ++ "}", "\\label{fig:my_label}", "\\end{figure}"]
  where
    aux Node{rootLabel=a,subForest=[]} = "\\textbf{" ++ a ++ "}"
    aux Node{rootLabel=a,subForest=xs} = unlines $ ("[.\\textit{"++ a ++"}"):(map (indent . aux) $ reverse xs)++["]"]
