{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String

data CompKind = Prim
              | Ho
              | Vert
              | Incl
              | TriInv
              | TriHo
              | Tri
              | Quad
              | TriVert
              | Super
              | Deform
                deriving Show

ckMap :: M.Map Char CompKind
ckMap = M.fromList [ ('一', Prim)
                   , ('吅', Ho)
                   , ('吕', Vert)
                   , ('回', Incl)
                   , ('咒', TriInv)
                   , ('弼', TriHo)
                   , ('品', Tri)
                   , ('叕', Quad)
                   , ('冖', TriVert)
                   , ('+', Super)
                   , ('*', Deform) ]

data Decomp = D { fstOf :: Char
                , sndOf :: Char
                , compKindOf :: CompKind }
              deriving Show

getCK :: Char -> Maybe CompKind
getCK = flip M.lookup ckMap

decomp :: Parser (Maybe (Char, Decomp))
decomp = do
  spaces
  hanzi <- satisfy (not . isSpace)
  spaces >> many digit >> spaces
  compkind <-  satisfy (not . isSpace)
  spaces
  fstPt <-  satisfy (not . isSpace)
  spaces
  optional (char '?')
  spaces >> many digit >> spaces
  sndPt <-  satisfy (not . isSpace)
  spaces
  optional (char '?')
  spaces
  many (noneOf "\n")
  return $ ((,) hanzi) <$> D fstPt sndPt <$> (getCK compkind)

table :: Parser (M.Map Char Decomp)
table = M.fromList . catMaybes <$> decomp `endBy` char '\n'

wikimedia :: FilePath
wikimedia = "wikimedia_decomp.txt"

parseWikimedia :: IO (Maybe (M.Map Char Decomp))
parseWikimedia = do
  contents <- readFile wikimedia
  let decomps = runParser table () wikimedia contents
  case decomps of
   Left _ -> return Nothing
   Right dc -> return $ Just dc
  -- print (take 5 <$> M.toList <$> decomps)
