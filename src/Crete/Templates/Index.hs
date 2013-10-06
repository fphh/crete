{-# OPTIONS_GHC -F -pgmF trhsx #-}


module Crete.Templates.Index where

import Control.Monad.Reader (ask)
import Control.Monad

import qualified Data.Map as Map
import qualified Data.Text as Text

import Happstack.Server.HSP.HTML

import qualified Crete.Templates.Product as Product
import qualified Crete.Templates.Page as Page

import Crete.Store.Store
import Crete.Url.Url (Url(..), Sitemap(..))
import Crete.Type



routed :: Url -> RoutedServer XML
routed url = do
  title <- askCnf cnfPageName
  content <- chooseContent url
  maybe mzero (Page.template url title) content

chooseContent :: Url -> RoutedServer (Maybe XML)
chooseContent (WithLang _ (Page str)) = do
  cm <- ask >>= getContentMap
  let f txt = unXMLGenT <div class=(str)><% cdata txt %></div>
  maybe (return Nothing) (fmap Just . f) (Map.lookup str cm)


chooseContent (WithLang _ (Products ln n)) =
  fmap Just $ Product.content (Text.unpack ln) n

chooseContent _ = return Nothing
