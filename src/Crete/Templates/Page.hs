{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -F -pgmFtrhsx #-}


module Crete.Templates.Page where

import Happstack.Server.HSP.HTML

import qualified Crete.Templates.Menu as Menu
import qualified Crete.Templates.Search as Search

import Crete.Url.Url
import Crete.Type


template :: 
  (EmbedAsChild RoutedServer title, EmbedAsChild RoutedServer content) =>
  Url -> title -> content -> RoutedServer (XMLType RoutedServer)
template url title content = do
  logo <- askCnf cnfLogoPic

  header <- Menu.header url
  search <- Search.content
  footer <- Menu.footer

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
              <img src=("/img/" ++ logo)/></a>
          </td>
        </tr>
      </table>
    </div>

    <% header %>

    <% search %>

    <div class="content">
        <% content %>
    </div>

    <% footer %>

    </div>

 
    </body>
    </html>