{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -F -pgmFtrhsx #-}


module Crete.Templates.Page where

import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Reader (ask, MonadReader)

import Data.Time (getCurrentTime)
import qualified Data.Map as Map

import Happstack.Server
import Happstack.Server.HSP.HTML

import qualified Crete.Templates.Menu as Menu
import Crete.Url.Url
import Crete.Store.Store
import Crete.Type


template :: 
  (MonadReader Config m, ServerMonad m,
   MonadIO m, XMLGenerator m,
   EmbedAsChild m title, EmbedAsChild m content) =>
  Url -> title ->  content -> m (XMLType m)
template url title content = do
  time <- liftIO $ getCurrentTime
  cm <- ask >>= getContentMap
  let menuItems =
        case map Page (Map.keys cm) of
             [] -> [Products]
             (x:xs) -> x : Products : xs
  unXMLGenT
    <html>

    <head>
      <title><% title %></title>
      <link rel="stylesheet" type="text/css" href="/css/format.css"/>
      <link rel="SHORTCUT ICON" type="image/x-icon" href="/css/favicon.ico"/>
    </head>

    <body>

    <div class="container">

    <div class="logo">
      <table>
        <tr>
          <td class="logotext"><% title %></td>
          <td class="logopic">
            <a target="_blank" href="http://de.wikipedia.org/wiki/Kreta">
              <img src="/img/Cretaperif.png"/></a>
          </td>
        </tr>
      </table>
    </div>

    <div class="header">
      <% Menu.header menuItems url %>
    </div>

    <div class="content">
        <% content %>
    </div>
    <div class="footer">
      <table width="100%">
        <tr>
          <td><% Menu.footer url %></td>
          <td class="time"><% show time %></td>
        </tr>
      </table>
    </div>
    </div>

 
    </body>
    </html>