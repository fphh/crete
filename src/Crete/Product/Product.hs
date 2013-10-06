

module Crete.Product.Product where

import Text.ParserCombinators.Parsec (parse)

import Data.Maybe (catMaybes)
import Data.Either (partitionEithers)

import qualified Data.List as List
import qualified Data.Map as Map


import qualified Crete.Product.ProductParser as PP

import Crete.Store.StoreTypes (ProductMap, Product(..))
import Crete.Utility (trimm, readMaybe)

newtype ProductError = ProductError (Maybe [String]) deriving (Show)

parseProduct :: FilePath -> String -> ([String], ProductMap)
parseProduct file input = 
  case parse (PP.csvFile ',') file input of
       Right res ->
         let (ls, rs) = partitionEithers (map f res)
         in (catMaybes ls, Map.fromList rs)
       Left msg -> (["Irgendwas lief schief! Prüfen Sie das Format Ihrer Produktlisten ...", show msg], Map.empty)
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