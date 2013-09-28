{-# OPTIONS_GHC -F -pgmF trhsx #-}


module Crete.Templates.Impressum where

import Control.Monad.Reader

import Happstack.Server.HSP.HTML

import Crete.Type (RoutedServer, liftRouted, cnf, Cnf(..))




content :: RoutedServer XML
content = do
  config <- ask
  let c = cnf config
      name = cnfName c
      street = cnfStreet c
      city = cnfCity c
      country = cnfCountry c
      email = cnfEmail c
      fon = cnfFon c
      fax = cnfFax c

  liftRouted $ unXMLGenT
    <div class="text">
    <h1>Impressum</h1>

    Diese Seite wird betrieben von<br/><br/>

    <table>
      <tr><td><%name%></td></tr>
      <tr><td><%street%></td></tr>
      <tr><td><%city%></td></tr>
      <tr><td><%country%></td></tr>
    </table>
    <br/>

    <table>
      <tr><td>MAIL:</td><td><a href=("mailto:" ++ email)><%email%></a></td></tr>
      <tr><td>FON:</td><td><%fon%></td></tr>
      <tr><td>FAX:</td><td><%fax%></td></tr>
    </table>
    <br/>


    <h2>Informationen über diese Seite</h2>


    <p>Die orginale Version dieser Seite wurde am 11. August 2012 erstellt. Die Inhalte
    dieser Seite wurden nach bestem Wissen erarbeitet. Der
    Inhalt verlinkter Seiten wurde geprüft. Diese Seite speichert keine
    personenbezogenen Daten.
    </p>

    <p>Diese Seite wurde validiert:</p>

    <table>
    <tr>
    <td style="padding-right:20px;">
        <a href="http://validator.w3.org/check?uri=referer" target="_blank">
        <img style="border:0;width:88px;height:31px"
             src="http://www.w3.org/Icons/valid-xhtml10" 
             alt="Valid XHTML 1.0 Transitional" /></a>
    </td>
    <td>
        <a href="http://jigsaw.w3.org/css-validator/check/referer" target="_blank">
           <img style="border:0;width:88px;height:31px"
                src="http://jigsaw.w3.org/css-validator/images/vcss"
                alt="CSS ist valide!" /></a>
    </td>
    </tr>
    </table>
     

    </div>
