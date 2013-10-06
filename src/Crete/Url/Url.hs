{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Crete.Url.Url where

import Prelude hiding (id, (.))
import Control.Category (Category((.)))

import Data.Maybe
import Data.Data (Data, Typeable)

import Web.Routes.TH
import Web.Routes.Boomerang hiding (arg)
import Text.Boomerang.TH (derivePrinterParsers)

import qualified Data.Text as Text; import Data.Text (Text)
import qualified Data.List as List



imgDirectory, cssDirectory, markupDirectory, csvDirectory :: String
imgDirectory = "versioned/img"
cssDirectory = "versioned/css"
markupDirectory = "versioned/markup"
csvDirectory = "versioned/csv"

data Lang = English | German deriving (Show, Eq, Typeable, Read, Ord, Data)

$(derivePathInfo ''Lang)
$(derivePrinterParsers ''Lang)

data Sitemap = Page Text
             | Products Text Int
             | LoginPage
             | LogoutPage
             | LookToken
             | LoadProd
             | LoadMarkup
             | PublishProd
             | Restart deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePathInfo ''Sitemap)
$(derivePrinterParsers ''Sitemap)


data Url = WithLang Lang Sitemap deriving (Eq, Typeable, Ord, Read, Data, Show)

$(derivePathInfo ''Url)
$(derivePrinterParsers ''Url)

type UrlRouter = forall a. Router a (Url :- a)
type SitemapRouter = forall a. Router a (Sitemap :- a)

lit' :: String -> Router r r
lit' = lit . Text.pack



-- Only case sensitive parser?
sitemap :: SitemapRouter
sitemap =
  rPage           . (lit' "page" </> anyText)
  <> rProducts    . (lit' "products" </> anyText </> int)
  <> rLoginPage   . lit' "login"
  <> rLogoutPage  . lit' "logout"
  <> rLookToken   . lit' "look"
  <> rLoadProd    . lit' "loadprod"
  <> rLoadMarkup  . lit' "loadmarkup"
  <> rPublishProd . lit' "publishprod"
  <> rRestart     . lit' "restart"

de :: String
de = "de"

en :: String
en = "en"

defaultLang :: Lang
defaultLang = German

toLang :: String -> Lang
toLang lang
  | lang == de = German
  | lang == en = English
  | otherwise = defaultLang

fromLang :: Lang -> String
fromLang German = de
fromLang English = en

urlmap :: UrlRouter
urlmap = rWithLang . rGerman . lit' (fromLang German) </> sitemap
         <> rWithLang . rEnglish . lit' (fromLang English) </> sitemap
         <> rWithLang . rGerman . sitemap

slash :: Char  
slash = '/'

urlToStr :: Url -> String
urlToStr =
  List.intercalate [slash] . map Text.unpack . fromJust . unparseTexts urlmap

slashUrlToStr :: Url -> String
slashUrlToStr = (slash :) . urlToStr

gePage :: String -> Url
gePage = WithLang German . Page . Text.pack

geProduct :: String -> Url
geProduct = WithLang German . flip Products 0 . Text.pack

translate :: Url -> String
translate (WithLang German (Page str))    = Text.unpack str
translate (WithLang German (Products p _))    = Text.unpack p

translate (WithLang English (Page str))    = Text.unpack str
translate (WithLang English (Products p _))   = Text.unpack p

translate url = slashUrlToStr url

