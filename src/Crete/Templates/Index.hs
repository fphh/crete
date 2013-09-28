{-# OPTIONS_GHC -F -pgmF trhsx #-}


module Crete.Templates.Index where

import Control.Monad.Reader (ask)

import qualified Data.Map as Map

import Happstack.Server.HSP.HTML

import qualified Crete.Templates.Product as Product
import qualified Crete.Templates.Impressum as Impressum
import qualified Crete.Templates.Page as Page
import Crete.Store.Store

import Crete.Url.Url (Url(..), Sitemap(..))
import Crete.Type (RoutedServer, liftRouted)



routed :: Url -> RoutedServer XML
routed url = do
  content <- chooseContent url
  Page.template url "Produkte aus Kreta" content


chooseContent :: Url -> RoutedServer XML
chooseContent (WithLang _ (Page str)) = do
  config <- ask
  cm <- getContentMap config
  let f txt = liftRouted $ unXMLGenT <div><% cdata txt %></div>
  maybe Product.content f (Map.lookup str cm)

chooseContent (WithLang _ Products) = Product.content
chooseContent (WithLang _ Impressum) = Impressum.content

chooseContent _ = liftRouted $ unXMLGenT
  <div>Keine Inhalte verfügbar.</div>
