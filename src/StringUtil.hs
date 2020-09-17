module StringUtil
  ( unlines
  , indent
  , gobble) where

import Prelude hiding (unlines)

indent s = unlines $ map (\s -> unwords ["   ", s]) $ lines s
unlines [] = []
unlines ([] : []) = []
unlines ([] : ss) = '\n' : unlines ss
unlines ((x : xs) : ss) = x : unlines (xs : ss)

gobble :: String -> String -> Maybe String
gobble (i:is) (s:ss)
  | i == s = gobble is ss
  | otherwise = Nothing
gobble is [] = Just is
gobble [] _ = Nothing