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

import qualified Data.Text as T

imgDirectory, cssDirectory, markupDirectory, csvDirectory :: String
imgDirectory = "versioned/img"
cssDirectory = "versioned/css"
markupDirectory = "versioned/markup"
csvDirectory = "versioned/csv"

data Lang = English | German deriving (Show, Eq, Typeable, Read, Ord, Data)

$(derivePathInfo ''Lang)
$(derivePrinterParsers ''Lang)

data Sitemap = Page T.Text
             | Products
             | Impressum
             | LoginPage
             | LogoutPage
             | LookToken
             | LoadProd
             | LoadMarkup
             | PublishProd deriving (Eq, Ord, Read, Show, Data, Typeable)



$(derivePathInfo ''Sitemap)
$(derivePrinterParsers ''Sitemap)


data Url = WithLang Lang Sitemap deriving (Eq, Typeable, Ord, Read, Data, Show)

$(derivePathInfo ''Url)
$(derivePrinterParsers ''Url)

type UrlRouter = forall a. Router a (Url :- a)
type SitemapRouter = forall a. Router a (Sitemap :- a)

lit' :: String -> Router r r
lit' = lit . T.pack



-- Only case sensitive parser?
sitemap :: SitemapRouter
sitemap =
  rPage           . (lit' "page" </> anyText)
  <> rProducts    . lit' "products"
  <> rImpressum   . lit' "impressum"
  <> rLoginPage   . lit' "login"
  <> rLogoutPage  . lit' "logout"
  <> rLookToken   . lit' "look"
  <> rLoadProd    . lit' "loadprod"
  <> rLoadMarkup  . lit' "loadmarkup"
  <> rPublishProd . lit' "publishprod"

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

--defaultLangSite :: Url
--defaultLangSite = WithLang defaultLang Home

defaultPage :: Url
defaultPage = WithLang German Products

urlmap :: UrlRouter
urlmap = rWithLang . rGerman . lit' (fromLang German) </> sitemap
         <> rWithLang . rEnglish . lit' (fromLang English) </> sitemap
         <> rWithLang . rGerman . sitemap

slash :: T.Text  
slash = T.pack "/"

urlToStr :: Url -> T.Text
urlToStr url = T.intercalate slash (fromJust $ unparseTexts urlmap url)

slashUrlToStr :: Url -> T.Text
slashUrlToStr url =
  T.append slash $ T.intercalate slash (fromJust $ unparseTexts urlmap url)


translate :: Url -> String
translate (WithLang German (Page str))    = T.unpack str
translate (WithLang German Products)    = "PRODUKTE"
translate (WithLang German Impressum)   = "IMPRESSUM"

translate (WithLang English (Page str))    = T.unpack str
translate (WithLang English Products)   = "PRODUCTS"
translate (WithLang English Impressum)  = "IMPRESSUM"

translate url = T.unpack $ slashUrlToStr url
