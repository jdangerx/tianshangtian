{-# LANGUAGE OverloadedStrings #-}
module Accrete where

import Data.Aeson
import Control.Monad
import Data.List (intersect)
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust, catMaybes, mapMaybe)
import qualified Data.Tree as T
import System.Random (randomRIO)
import qualified Data.Text as T

import Parse

-- Components are the basic-est building blocks of characters.
-- Currently no support for inside/outside relations.
-- P : primitive, UD : up/down split, LR : left/right split
data Component = P Char
               | UD
               | LR
                 deriving (Show, Eq)

instance ToJSON Component where
  toJSON (P c) = String $ T.pack [c]
  toJSON LR = String "LR"
  toJSON UD = String "UD"

data Branch = Left | Right

type SuperChar = T.Tree Component

-- Combine can only recurse deeper if we're not stacking LR's on LR's
-- or UD's on UD's.  This tries to combine something with the two
-- subtrees of an old superchar; if that works then we return the new
-- subtree. Otherwise return `Nothing`
subcombine :: SuperChar -> SuperChar -> Maybe SuperChar
subcombine new@(T.Node LR _) old@(T.Node UD [a, b]) =
  let newA = combine new a
      newB = combine new b
  in
   if isJust newA  then Just $ old {T.subForest = [fromJust newA, b]}
   else if isJust newB then Just $ old {T.subForest = [a, fromJust newB]}
        else Nothing
subcombine new@(T.Node UD _) old@(T.Node LR [a, b]) =
  let newA = combine new a
      newB = combine new b
  in
   if isJust newA  then Just $ old {T.subForest = [fromJust newA, b]}
   else if isJust newB then Just $ old {T.subForest = [a, fromJust newB]}
        else Nothing
subcombine _ _ = Nothing

combine :: SuperChar -> SuperChar -> Maybe SuperChar
combine (T.Node (P _) _) _ = Nothing
combine new@(T.Node LR [l, r]) old =
  case old of
   T.Node rl@(P _) _ -> if T.rootLabel l == rl || T.rootLabel r == rl
                       then Just new else Nothing
   T.Node LR [l', r'] | l == r' -> Just $ T.Node LR [old, r]
                      | r == l' -> Just $ T.Node LR [l, old]
                      | otherwise -> subcombine new old
   T.Node UD [u', d'] | l == u' || l == d' -> Just $ T.Node LR [old, r]
                      | r == u' || r == d' -> Just $ T.Node LR [l, old]
                      | otherwise -> subcombine new old
combine new@(T.Node UD [u, d]) old =
  case old of
   T.Node rl@(P _) _ -> if T.rootLabel u == rl || T.rootLabel d == rl
                       then Just new else Nothing
   T.Node LR [l', r'] | u == l' || u == r' -> Just $ T.Node UD [old, d]
                      | d == l' || d == r' -> Just $ T.Node UD [u, old]
                      | otherwise -> subcombine new old
   T.Node UD [u', d'] | u == d' -> Just $ T.Node UD [old, d]
                      | d == u' -> Just $ T.Node UD [u, old]
                      | otherwise -> subcombine new old
combine _ _ = Nothing

-- Turn a decomp into a SuperChar, which is a Tree of Components.
expand :: Decomp -> Maybe SuperChar
expand (D {compKindOf = Prim, fstOf = c}) = Just $ T.Node (P c) []
expand (D {compKindOf = cmp, fstOf = c, sndOf = c'}) =
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

-- simplify what we can of the decomps map and pull it out into a new map
simplifyDecomps :: M.Map Char Decomp -> M.Map Char SuperChar
simplifyDecomps = M.map fromJust . M.filter isJust . M.map expand

-- get primitives in a SuperChar
prims :: SuperChar -> String
prims sc =
  let
    toChar (P c) = Just c
    toChar _ = Nothing
  in mapMaybe toChar . T.flatten $ sc

-- random choice fn
choice :: [a] -> IO (Maybe a)
choice [] = pure Nothing
choice ls = Just <$> liftM (ls !!) (randomRIO (0, length ls - 1))

-- make a new SuperChar (maybe) from a superchar by adding a new
-- chinese character to it. Also append to a string if it worked.
next :: M.Map Char SuperChar
     -> (SuperChar, String)
     -> IO (Maybe (SuperChar, String))
next m (c, ls) =
  let
    cPrims = prims c
    potSCs = M.filter (\sc -> prims sc `intersect` cPrims /= []) m
    potNewTrees = catMaybes
                  (
                    (\(char, sc) -> liftM2 (,) (sc `combine` c) (Just char))
                    <$> M.toList potSCs
                  )
  in
   do
     choiceM <- choice potNewTrees
     case choiceM of
      Nothing -> return Nothing
      Just (nextSuperChar, nextChar) ->
        return $ Just (nextSuperChar, nextChar : ls)

-- keep trying to stick new chars onto a superchar until we can't
-- anymore or we hit the max depth.
accrete :: M.Map Char SuperChar
        -> (SuperChar, String) -- state
        -> Int -- maxdepth
        -> IO (SuperChar, String)
accrete _ c 0 = return c
accrete m c n = do
  smaller <- accrete m c (n-1)
  nextCharM <- next m smaller
  case nextCharM of
   Nothing -> return smaller
   Just nextChar -> return nextChar

-- convert to something we can print good with T.drawTree
toStrTree :: SuperChar -> T.Tree String
toStrTree =
  let
    convert (P c') = [c']
    convert LR = "LR"
    convert UD = "UD"
  in
   (<$>) convert
