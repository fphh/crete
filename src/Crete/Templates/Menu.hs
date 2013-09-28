{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -F -pgmF trhsx #-}

module Crete.Templates.Menu where

import Happstack.Server.HSP.HTML

import Crete.Url.Url (Lang(..), Sitemap(..), Url(..), urlToStr, translate)

import qualified Data.Text as Text


footerItems :: [Sitemap]
footerItems = [Impressum]

footerEnglish :: [Url]
footerEnglish =  map (WithLang English) footerItems

footerGerman :: [Url]
footerGerman = map (WithLang German) footerItems


bar :: (XMLGenerator m, EmbedAsChild m String) =>
  [Url] -> Url -> XMLGenT m [ChildType m]
bar items url =
  <%>
  <table>
  <tr><% map f items %></tr>
  </table>
  </%>
  where w = (60 +) . (10 *) . length 
        f thisurl | thisurl == url = 
          <td id="active" width=(w $ translate thisurl)>
          <% translate thisurl %></td>
        f thisurl =
          <td  width=(w $ translate thisurl)>
          <a href=("/" ++ (Text.unpack $ urlToStr thisurl))>
          <% translate thisurl %>
          </a>
          </td>

header ::
  (XMLGenerator m) => [Sitemap] -> Url -> XMLGenT m [ChildType m]
header items url = bar (map (WithLang German) items) url

footer ::
  (XMLGenerator m) => Url -> XMLGenT m [ChildType m]
footer = bar footerGerman
