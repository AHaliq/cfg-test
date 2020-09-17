module Cfg 
  ( CFG
  , empty
  , fromRuleList
  , newRule
  , getRules
  , getStart
  , isNonTerminal
  , isTerminal
  )where


import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

data CFG = CFG (Set String) (Set String) (Map String [[String]]) String   deriving (Show, Eq)
empty s = if isNonTerminal s then Just $ CFG (Set.singleton s) Set.empty Map.empty s else Nothing
fromRuleList s (r:rs) = fromRuleList s rs >>= (\cfg -> newRule r cfg)
fromRuleList s [] = empty s
isNonTerminal (x:xs) = isAlpha x && isUpper x
isTerminal s = not $ isNonTerminal s
newRule str (CFG v s r s0) = case words str of
    (nt:"::=":ts) -> if isTerminal nt then Nothing else
                    let (v',s') = updateSet v s (nt:ts) in
                        Just (CFG v' s' (Map.insertWith (++) nt [ts] r) s0)    
    _ -> Nothing
updateSet v s ts = foldl (\(v,s) x -> if isNonTerminal x then (Set.insert x v, s) else (v, Set.insert x s)) (v,s) ts

getRules nt (CFG _ _ r _) = case Map.lookup nt r of Just rs -> rs; Nothing -> []

getStart (CFG _ _ _ s) = s