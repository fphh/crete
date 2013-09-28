

module Crete.Product.Product where

import Text.ParserCombinators.Parsec (parse)

import Data.Maybe (catMaybes)
import Data.Either (partitionEithers)

import qualified Data.List as List
import qualified Data.Map as Map


import qualified Crete.Product.ProductParser as PP

import Crete.Store.StoreTypes (ProductMap, Product(..))
import Crete.Utility (trimm)

{-
checkPrice :: String -> String
checkPrice str = euro ++ "." ++ reverse cent
  where rstr = reverse str
        cent = takeWhile p rstr
        euro = takeWhile p str
        p c = c /= '.' && c /= ','
-}

newtype ProductError = ProductError (Maybe [String]) deriving (Show)

readMaybe :: Read a => String -> Maybe a
readMaybe s =
  case [x | (x,t) <- reads s, ("","") <- lex t] of
       [x] -> Just x
       _ -> Nothing

parseProduct :: FilePath -> String -> ([String], ProductMap)
parseProduct file input = 
  case parse (PP.csvFile ',') file input of
       Right res ->
         let (ls, rs) = partitionEithers (map f res)
         in (catMaybes ls, Map.fromList rs)
       Left msg -> (["Irgendwas lief schief! Prüfen Sie das Format Ihrer Produktliste...", show msg], Map.empty)
  where g ',' = '.'
        g x = x
        f xs@[name, qty, desc, pic, unit, price] = 
          case (readMaybe qty, readMaybe (map g price)) of
               (Just q, Just p) ->
                 Right $ (,)
                   (trimm name) 
                   (Product q (trimm desc) (trimm pic) (trimm unit) p)
               _ -> Left $ errorLine xs
        f [] = Left Nothing
        f xs = Left $ errorLine xs
        errorLine xs = Just $ '"' : List.intercalate "\", \"" xs ++ ['"']