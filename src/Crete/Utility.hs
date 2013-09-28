

module Crete.Utility where

import qualified Data.Map as Map; import Data.Map (Map)

import Data.Char (isSpace)


trimm :: String -> String
trimm = reverse . dropWhile isSpace . reverse . dropWhile isSpace


escape :: String -> String
escape = concatMap (\c -> maybe (c:[]) id (Map.lookup c umlaute))


umlaute :: Map Char String
umlaute = Map.fromList $
  ('ä', "&auml;") :
  ('Ä', "&Auml;") :
  ('ö', "&ouml;") :
  ('Ö', "&Ouml;") :
  ('ü', "&uuml;") :
  ('Ü', "&Uuml;") :
  ('ß', "&szlig;") : []
