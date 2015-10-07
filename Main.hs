{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Text.Lazy (splitOn, pack)
import Data.Tree (drawTree)
import Web.Scotty

import Parse (parseWikimedia)
import Accrete

main :: IO ()
main = do
  decomps <- fromJust <$> (simplifyDecomps <$>) <$> parseWikimedia
  scotty 3000 $
    get "/:char/:num" $ do
      accept <- header "Accept"
      char <- param "char"
      num <- param "num"
      let superChar = M.lookup char decomps
      case superChar of
       Nothing -> text "no such char found"
       Just sc ->
         case accept of
          Nothing -> text "You don't want anything, and I'm not gonna give it to you."
          Just something ->
            do
              (complete, chars) <- liftIO (accrete decomps (sc, [char]) num)
              if "text/html" `elem` splitOn "," something
                then text $ pack . (++ reverse chars) . drawTree . toStrTree $ complete
                else Web.Scotty.json complete
