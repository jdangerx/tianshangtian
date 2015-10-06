module Accrete where

import Control.Monad
import Data.List (intersect)
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust, catMaybes, mapMaybe)
import qualified Data.Tree as T
import System.Random (randomRIO)

import Parse

data Component = P Char
               | UD
               | LR
                 deriving (Show, Eq)

type SuperChar = T.Tree Component

foo :: SuperChar -> SuperChar -> SuperChar -> SuperChar -> Maybe SuperChar
foo new old a b =
  let newA = combine new a
      newB = combine new b
  in
   if isJust newA  then Just $ old {T.subForest = [fromJust newA, b]}
   else if isJust newB then Just $ old {T.subForest = [a, fromJust newB]}
        else Nothing

combine :: SuperChar -> SuperChar -> Maybe SuperChar
combine (T.Node (P _) _) old = Nothing
combine new@(T.Node LR [l, r]) old@(T.Node _ [a, b]) =
  case old of
   T.Node rl@(P _) _ -> if T.rootLabel l == rl || T.rootLabel r == rl
                       then Just new else Nothing
   T.Node LR [l', r'] | l == r' -> Just $ T.Node LR [old, r]
                      | r == l' -> Just $ T.Node LR [l, old]
                      | otherwise -> foo new old a b
   T.Node UD [u', d'] | l == u' || l == d' -> Just $ T.Node LR [old, r]
                      | r == u' || r == d' -> Just $ T.Node LR [l, old]
                      | otherwise -> foo new old a b
combine new@(T.Node UD [u, d]) old@(T.Node _ [a, b]) =
  case old of
   T.Node rl@(P _) _ -> if T.rootLabel u == rl || T.rootLabel d == rl
                       then Just new else Nothing
   T.Node LR [l', r'] | u == l' || u == r' -> Just $ T.Node UD [old, d]
                      | d == l' || d == r' -> Just $ T.Node UD [u, old]
                      | otherwise -> foo new old a b
   T.Node UD [u', d'] | u == d' -> Just $ T.Node UD [old, d]
                      | d == u' -> Just $ T.Node UD [u, old]
                      | otherwise -> foo new old a b
combine _ _ = Nothing


expand :: M.Map Char Decomp -> Decomp -> Maybe SuperChar
expand _ (D {compKindOf = Prim, fstOf = c}) = Just $ T.Node (P c) []
expand _ (D {compKindOf = cmp, fstOf = c, sndOf = c'}) =
  let
    cLeaf = T.Node (P c) []
    c'Leaf = T.Node (P c') []
  in
   case cmp of
    Ho -> Just . T.Node LR $ [cLeaf, c'Leaf]
    Vert -> Just . T.Node UD $ [cLeaf, c'Leaf]
    TriInv -> Just . T.Node UD $ [T.Node LR [cLeaf, cLeaf], c'Leaf]
    TriHo -> Just . T.Node UD $ [c'Leaf, T.Node LR [cLeaf, cLeaf]]
    Tri -> Just . T.Node UD $ [cLeaf, T.Node LR [cLeaf, cLeaf]]
    Quad -> Just . T.Node UD $
           [T.Node LR [cLeaf, cLeaf], T.Node LR [cLeaf, cLeaf]]
    TriVert -> Just . T.Node UD $ [T.Node UD [cLeaf, T.Node (P '冖') []], c'Leaf]
    _ -> Nothing

expand' :: Decomp -> Maybe SuperChar
expand' = expand M.empty

simplifyDecomps :: M.Map Char Decomp -> M.Map Char SuperChar
simplifyDecomps = M.map fromJust . M.filter isJust . M.map expand'

prims :: SuperChar -> String
prims sc =
  let
    toChar (P c) = Just c
    toChar _ = Nothing
  in mapMaybe toChar . T.flatten $ sc

choice :: [a] -> IO a
choice ls = liftM (ls !!) (randomRIO (0, length ls - 1))

next :: M.Map Char SuperChar -> SuperChar -> IO SuperChar
next m c =
  let
    cPrims = prims c
    potSCs = M.elems . M.filter (\sc -> prims sc `intersect` cPrims /= []) $ m
    potNewTrees = catMaybes ((`combine` c) <$> potSCs)
  in choice potNewTrees

accrete :: M.Map Char SuperChar -> SuperChar -> Int -> IO SuperChar
accrete m c 0 = return c
accrete m c n = (accrete m c (n-1)) >>= (next m)

main :: IO ()
main = do
  decomps <- (simplifyDecomps <$>) <$> parseWikimedia
  let seed = fromJust . join $ (M.lookup '和' <$> decomps)
  tree5 <- accrete (fromJust decomps) seed 2
  let strTree' = (\c -> case c of
                        P c' -> [c']
                        LR -> "LR"
                        UD -> "UD") <$> tree5
  putStrLn $ T.drawTree strTree'
