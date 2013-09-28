{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Crete.Stats.Stats where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader


import Data.Time (getCurrentTime)
import qualified Data.Hashable as H
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS

import Crypto.Hash.SHA512 (hash)

import Happstack.Server
import HSP
import Web.Routes.XMLGenT ()

import Text.Reform 
import Text.Reform.Happstack
import Text.Reform.HSP.String

import Crete.Type
import Crete.Url.Url
import Crete.Store.Store
import Crete.Store.StoreTypes (LoginToken(..), ProductMap, Product(..))
import qualified Crete.Templates.Page as Page




type Password = String

newtype Login = Login Password deriving (Show)

data LoginError = InvalidPassword
                | AppCFE (CommonFormError [Input]) deriving (Show)


instance FormError LoginError where
    type ErrorInputType LoginError = [Input]
    commonFormError = AppCFE


type SimpleForm m =
  Form m [Input] LoginError [XMLGenT m XML] ()


cretetoken :: String
cretetoken = "cretetoken"

cookieLife :: CookieLife
cookieLife = MaxAge (60 * 10)


checkPassword :: String -> Either LoginError String
checkPassword "abc123" = Right "bla"
checkPassword _ = Left InvalidPassword


-- newtype ProductFile = ProductFile FilePath deriving (Show)

--fileUpload :: SimpleForm RoutedServer ProductFile
--fileUpload =
--  ProductFile <$> inputFile <* inputSubmit "Submit"

loginForm :: SimpleForm RoutedServer Login
loginForm =
  Login <$> lgin <*  inputSubmit "Submit"
  where lgin = -- errors listErrors ++> label "Login:" ++>
               (inputText "" `transformEither` checkPassword)

renderLogin ::
  ( XMLGen m, 
    EmbedAsChild m Password,
    EmbedAsChild m (XMLType m)) =>
  Login -> XMLGenT m (XMLType m)
renderLogin (Login pwd) = do
  <table>
    <tr><td>mmLogin:</td><td><% pwd %></td></tr>
  </table>

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)


checkLogin :: RoutedServer ()
checkLogin = do
  LoginToken tok <- ask >>= getLoginToken
  cv <- lookCookieValue cretetoken
  when (tok /= Just cv) mzero

login :: Login -> RoutedServer XML
login (Login str) = do
  time <- liftIO $ getCurrentTime
  let h = show $ H.hash $ hash $ BS.pack $ show time ++ str
  config <- ask
  setLoginToken config h
  addCookie cookieLife (mkCookie cretetoken h)
  adminTemplate (WithLang German LoginPage) "Logged in ..."


adminTemplate ::
  EmbedAsChild RoutedServer result => Url -> result -> RoutedServer XML
adminTemplate url res =
  Page.template url "Verwaltung" $
    <div>
      <h1>Aktionen</h1>
      <% todoList %>
      <h1>Ergebnis der letzten Aktion</h1>
      <% res %>
    </div>

todoList :: XMLGenT RoutedServer (XMLType RoutedServer)
todoList =
    <ul>
    <li><a href=(slashUrlToStr (WithLang German LookToken))>Look token</a></li>
    <li><a href=(slashUrlToStr (WithLang German LoadProd))>Produktliste neu laden</a></li>
    <li><a href=(slashUrlToStr (WithLang German LoadMarkup))>Seiten neu laden</a></li>
    <li><a href=(slashUrlToStr (WithLang German LogoutPage))>Logout</a></li>

    </ul>

loginPage :: RoutedServer XML
loginPage = do
  decodeBody myPolicy
  let nextpage = form $ urlToStr (WithLang German LoginPage)
  res <- happstackEitherForm nextpage "fieldId" loginForm
  case res of
    (Left formHtml) ->
      Page.template (WithLang German LoginPage) "Verwaltung" formHtml
    (Right l) -> login l


productListError :: [String] -> XMLGenT RoutedServer (XMLType RoutedServer)
productListError [] =
  <div>
  <h2>Produktliste in Ordnung</h2>
  <a href=(slashUrlToStr (WithLang German PublishProd))>Veröffentlichen</a>
  </div>
productListError es =
  <div>
  <h2><font color="#FF0000">Fehler in der Produktliste</font></h2>
  <ul>
    <li>Haben Sie eine csv-Datei übermittelt?</li>
    <li>Sind die Spalten durch Kommas getrennt?</li>
    <li>Steht der Text in den Zellen in Gänsefüßchen?</li>
  </ul>
  <h2><font color="#FF0000">Folgende Spalten enthalten Fehler</font></h2>
  <ul><% map (\x -> <li><% x %></li>) es %></ul>
  </div>


productListPreview :: ProductMap -> XMLGenT RoutedServer (XMLType RoutedServer)
productListPreview pm | Map.null pm = <div></div>
productListPreview pm =
  <div>
  <h2>Vorschau der Produktliste</h2>
  <% Map.foldlWithKey f emptyTable pm %>
  </div>
  where emptyTable =
          <table  class="products" rules="rows">
            <tr class="productline"> 
                 <td><b>Name</b></td>
                 <td><b>Menge</b></td>
                 <td><b>Beschreibung</b></td>
                 <td><b>Bild</b></td>
                 <td><b>Einheit</b></td>
                 <td><b>Preis</b></td> </tr>
          </table>

        f t name p = t <:
          (<tr class="productline">
                <td><% name %></td>
                <td><% show (productQuantity p) %></td>
                <td><% productDescription p %></td>
                <td> <img src=("/img/" ++ productPicture p) width="120px"/>
                     <% productPicture p %> </td>
                <td><% productUnit p %></td>
                <td><% show (productPrice p) %></td> </tr>)

productList ::
  ([String], ProductMap) -> XMLGenT RoutedServer (XMLType RoutedServer)
productList (es, pm) = 
  <div>
  <% productListError es %>
  <% productListPreview pm %>
  </div>
