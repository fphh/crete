{-# OPTIONS_GHC -F -pgmF trhsx #-}


module Crete.Templates.Product where

import Control.Monad.Reader

import Happstack.Server.HSP.HTML
import Web.Routes.Happstack ()

import Crete.Store.StoreTypes (Product(..), ListName, ProductName)
import Crete.Store.Store (getProductListMap)
import Crete.Type (RoutedServer, liftRouted, cnf, Cnf(..), askCnf, Config)
import Crete.Url.Url

import qualified Data.Map as Map
import qualified Data.Text as Text

mailto :: String -> String -> String
mailto email prod =
  "mailto:" ++ email
  ++ "?subject=Anfrage%20bezüglich%20" ++ prod

{-
paypalAddr :: String
paypalAddr = "https://www.sandbox.paypal.com"

paypalBusiness :: String
paypalBusiness = "hoerdegen-facilitator@laposte.net"


buybutton :: 
  (XMLGenerator m) =>
  String -> String -> String -> XMLGenT m [ChildType m]
buybutton prod price email =
  <%>
  <form target="paypal" action=(paypalAddr ++ "/cgi-bin/webscr") method="post" >
  <input type="hidden" name="cmd" value="_cart"/>
  <input type="hidden" name="business" value=paypalBusiness />
  <input type="hidden" name="lc" value="DE"/>
  <input type="hidden" name="item_name" value=prod/>
  <input type="hidden" name="amount" value=price/>
  <input type="hidden" name="currency_code" value="EUR"/>
  <input type="hidden" name="button_subtype" value="products"/>
  <input type="hidden" name="no_note" value="0"/>
  <input type="hidden" name="shipping" value="3.00"/>
  <input type="hidden" name="add" value="1"/>
  <input type="hidden" name="notify_url" value=""/>
  <input type="image" src="https://www.paypalobjects.com/de_DE/DE/i/btn/btn_cart_LG.gif" border="0" name="submit" alt="Jetzt einfach, schnell und sicher online bezahlen – mit PayPal."/>
  <img alt="" border="0" src="https://www.paypalobjects.com/de_DE/i/scr/pixel.gif" width="1" height="1"/>
  </form>
  </%>
-}

buybutton :: 
  (XMLGenerator m) => Cnf -> String -> Double -> XMLGenT m [ChildType m]
buybutton conf prod price =
  <%>
  <form action=(cnfPPAddress conf ++ "/cgi-bin/webscr") method="post" target="paypal">
  <input type="hidden" name="cmd" value="_xclick" />
  <input type="hidden" name="business" value=(cnfPPBusiness conf) />
  <input type="hidden" name="lc" value="DE"/>
  <input type="hidden" name="item_name" value=prod/>
  <input type="hidden" name="amount" value=(show price)/>
  <input type="hidden" name="currency_code" value=(cnfPPCurrency conf)/>
  <input type="hidden" name="button_subtype" value="products"/>
  <input type="hidden" name="no_note" value="0"/>
  <input type="hidden" name="shipping" value=(cnfPPShipping conf)/>
  <input type="image" src="https://www.paypalobjects.com/de_DE/DE/i/btn/btn_buynow_LG.gif" border="0" name="submit" alt="Jetzt einfach, schnell und sicher online bezahlen – mit PayPal." />
--  <img alt="" border="0" src="https://www.paypalobjects.com/en_US/i/scr/pixel.gif" width="1" height="1" />
  </form>
  </%>


outOfStock :: (a, Product) -> Bool
outOfStock = (0 /=) . productQuantity . snd

productLine ::
  (XMLGenerator m) =>
  String -> Cnf -> (ProductName, Product) -> XMLGenT m (XMLType m)
productLine  email c (prod, Product _ desc picture unit price) = 
        <tr class="productline">
          <td class="productname"><%prod%></td>
          <td class="description">
            <%desc%><br/><br/>
            <b><%unit%></b> zu <b><% show price %>EUR</b><br/>
          </td>
          <td class="price">
            <a href=("/img/" ++ picture) target="_blank">
              <img src=("/img/" ++ picture)/></a><br/><br/>
             <% buybutton c prod price %>
            <a href=(mailto email prod)>Eine Anfrage senden</a>
          </td>
        </tr>

content :: ListName -> Int -> RoutedServer XML
content lname n = do
  config <- ask
  prods <- getProductListMap config
  email <- askCnf cnfEmail
  chunkSize <- askCnf cnfChunkSize
  let look k =
        maybe (error $ "content: Missing product list \"" ++ lname ++ "\"")
              id
              (Map.lookup k prods)

      lst = Map.toList $ look lname
      len = length lst
      cs = len `div` chunkSize
      r =  len `rem` chunkSize
      ns = take (if r == 0 then cs else cs+1) $ iterate (+chunkSize) 0

      mkUrl x = slashUrlToStr $ WithLang German (Products (Text.pack lname) x)

      g x | x == n = <td><% show x %></td>
      g x = <td><a href=(mkUrl x)><% show x %></a></td>

      nexttable =
        case ns of
             (_:_:_) ->
               [ <hr/>,
                 <center>
                 <table class="next">
                 <tr><% map g ns %></tr>
                 </table>
                 </center> ]
             _ -> []

  liftRouted $ unXMLGenT
    <div>
    <h1>Produktkategorie <em><% lname %></em></h1>
    <table class="products" rules="rows">
    <% map (productLine email (cnf config)) 
       $ filter outOfStock
       $ take chunkSize
       $ drop n lst %>
    </table>
    <% nexttable %>
    </div>
