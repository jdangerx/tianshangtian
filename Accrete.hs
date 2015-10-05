module Accrete where

import qualified Data.Map as M
import Data.Maybe (isJust, fromJust, catMaybes)
import qualified Data.Tree as T
import Control.Monad
import Parse


data Component = P Char
               | UD
               | LR
                 deriving (Show, Eq)

type SuperChar = T.Tree Component

combine :: SuperChar -> SuperChar -> SuperChar
combine (T.Node (P _) _) old = old
combine new@(T.Node LR [l, r]) old@(T.Node _ [a, b]) =
  case old of
   T.Node rl@(P _) _ -> if T.rootLabel l == rl || T.rootLabel r == rl
                       then new else old
   T.Node LR [l', r'] | l == r' -> T.Node LR [old, r]
                      | r == l' -> T.Node LR [l, old]
                      | otherwise -> old
   T.Node UD [u', d'] | l == u' || l == d' -> T.Node LR [old, r]
                      | r == u' || r == d' -> T.Node LR [l, old]
                      | otherwise ->
                        let newA = combine new a
                            newB = combine new b
                        in
                         if newA /= a then old {T.subForest = [newA, b]}
                         else if newB /= b then old {T.subForest = [a, newB]}
                              else old
combine new@(T.Node UD [u, d]) old@(T.Node _ [a, b]) =
  case old of
   T.Node rl@(P _) _ -> if T.rootLabel u == rl || T.rootLabel d == rl
                       then new else old
   T.Node LR [l', r'] | u == l' || u == r' -> T.Node UD [old, d]
                      | d == l' || d == r' -> T.Node UD [u, old]
                      | otherwise -> old
   T.Node UD [u', d'] | u == d' -> T.Node UD [old, d]
                      | d == u' -> T.Node UD [u, old]
                      | otherwise ->
                        let newA = combine new a
                            newB = combine new b
                        in
                         if newA /= a then old {T.subForest = [newA, b]}
                         else if newB /= b then old {T.subForest = [a, newB]}
                              else old
combine _ old = old


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
    TriVert -> Just . T.Node UD $ [cLeaf, T.Node UD [T.Node (P '冖') [], c'Leaf]]
    _ -> Nothing

expand' :: Decomp -> Maybe SuperChar
expand' = expand M.empty

simplifyDecomps :: M.Map Char Decomp -> M.Map Char SuperChar
simplifyDecomps = M.map fromJust . M.filter isJust . M.map expand'

main :: IO ()
main = do
  decomps <- (simplifyDecomps <$>) <$> parseWikimedia
  let seedtree = fromJust . join $ (M.lookup '孔' <$> decomps)
  let tree'' = combine (fromJust . join $ (M.lookup '孟' <$> decomps)) seedtree
  let tree' = combine (fromJust . join $ (M.lookup '好' <$> decomps)) tree''
  -- let tree' = combine (fromJust . join $ (M.lookup '委' <$> decomps)) tree''
  -- let tree = combine (fromJust . join $ (M.lookup '妝' <$> decomps)) tree'
  let strTree = (\c -> case c of
                       P c' -> [c']
                       LR -> "LR"
                       UD -> "UD") <$> tree'
  putStrLn (T.drawTree strTree)
