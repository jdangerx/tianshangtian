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
combine new@(T.Node LR (l:r:[])) old =
  case old of
   T.Node rl@(P c) _ -> if T.rootLabel l == rl || T.rootLabel r == rl
                       then new else old
   T.Node LR (l':r':[]) -> if l == r'
                               then T.Node LR [old, new]
                               else if r == l'
                                    then T.Node LR [new, old]
                                    else old
   T.Node UD (u':d':[]) -> if l == u' || l == d'
                          then T.Node LR [old, new]
                          else if r == u' || r == d'
                               then T.Node LR [new, old]
                               else old
combine new@(T.Node UD (u:d:[])) old =
  case old of
   T.Node rl@(P c) _ -> if T.rootLabel u == rl || T.rootLabel d == rl
                       then new else old
   T.Node LR (l':r':[]) -> if u == l' || u == r'
                          then T.Node UD [old, new]
                               else if d == l' || d == r'
                                    then T.Node UD [new, old]
                                    else old
   T.Node UD (u':d':[]) -> if u == d'
                          then T.Node UD [old, new]
                          else if d == u'
                               then T.Node UD [new, old]
                               else old

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
  let tree = fromJust . join $ (M.lookup '受' <$> decomps)
  let strTree = (\c -> case c of
                       P c' -> [c']
                       LR -> "LR"
                       UD -> "UD") <$> tree
  putStrLn (T.drawTree strTree)
