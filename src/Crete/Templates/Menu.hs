{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -F -pgmF trhsx #-}

module Crete.Templates.Menu where

import Control.Monad.Reader
import Data.Time (getCurrentTime)

import Happstack.Server
--import Happstack.Server.HSP.HTML
import HSX.XMLGenerator

import Crete.Url.Url (Sitemap(..), Url(..), translate, gePage, geProduct, slashUrlToStr)
import Crete.Type

import qualified Data.Text as Text


adjustWidth :: [a] -> Int
adjustWidth = (60 +) . (10 *) . length

dropdownbar :: 
  (MonadReader Config m, ServerMonad m, Functor m,
   MonadIO m, XMLGenerator m) =>
  Url -> m (XMLType m)
dropdownbar url = do
  items <- map gePage `fmap` askCnf cnfPages
  subitems <- map geProduct `fmap` askCnf cnfProducts
  dp <- askCnf cnfProductPage
  let isProd (WithLang _ (Page str)) = str == Text.pack dp
      isProd _ = False

      go (WithLang _ (Products _ _)) thisurl | isProd thisurl =
               <li class="listmenu" id="active">
               <% translate thisurl %>
               <% submenu thisurl %>
               </li>
      go u thisurl | u == thisurl =
               <li class="listmenu" id="active">
               <% translate thisurl %>
               <% submenu thisurl %>
               </li>
      go _ thisurl | isProd thisurl =
               <li class="listmenu">
               <% translate thisurl %>
               <% submenu thisurl %>
               </li>
      go _ thisurl =
               <li class="listmenu">
               <a href=(slashUrlToStr thisurl)><% translate thisurl %></a>
               <% submenu thisurl %>
               </li>


      submenu u | isProd u =
        [<ul><% zipWith smitem [0::Int ..] subitems %></ul>]
      submenu _ = []

      smitem n u = 
        <li class="listsubmenu" id=("sub" ++ show n)>
        <div>
        <a href=(slashUrlToStr u)><% translate u %></a>
        </div>
        </li>

  unXMLGenT $
    <div class="dropdownheader">
    <ul>
    <% map (go url) items %>
    </ul>
    </div>

header :: 
  (MonadReader Config m, ServerMonad m, Functor m,
   MonadIO m, XMLGenerator m) =>
  Url -> m (XMLType m)
header url = dropdownbar url


footer :: 
  (MonadReader Config m, ServerMonad m, Functor m,
   MonadIO m, XMLGenerator m) =>
  m (XMLType m)
footer = do
  time <- liftIO $ getCurrentTime
  items <- map gePage `fmap` askCnf cnfFooters
  let f thisurl =
        <td width=(adjustWidth $ translate thisurl)>
        <a href=(slashUrlToStr thisurl)>
        <% translate thisurl %></a></td>
  unXMLGenT $
    <div class="footer">
    <table class="footermenu">
    <tr><% map f items %>
        <td class="time"><% show time %></td></tr>
    </table>
    </div>
