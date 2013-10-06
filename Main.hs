

module Main where

import Happstack.Server

import Control.Monad (msum)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (runReaderT, ask)
import Control.Exception (bracket)
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (takeMVar, putMVar)

import Data.Acid (AcidState, IsAcidic)
import Data.Acid.Local (openLocalStateFrom, createCheckpointAndClose)
import Data.Typeable
import qualified Data.Map as Map
import qualified Data.List as List

import Data.Maybe (catMaybes)

import Text.Pandoc (def, writeHtmlString, readMarkdown, readMediaWiki, readHtml)

import Happstack.Server.HSP.HTML (XML)

import System.FilePath             ((</>))


import Web.Routes (Site, setDefault, unRouteT)
import Web.Routes.Happstack (implSite)
import Web.Routes.Boomerang (boomerangSite)

import qualified Data.Text as T

import Crete.Type
import Crete.Url.Url
import Crete.Store.Store
import Crete.Store.StoreTypes
import Crete.Product.Product (parseProduct)
import Crete.Stats.Stats (loginPage, cretetoken, checkLogin, adminTemplate, productList)
import qualified Crete.Templates.Index as Index

import System.Directory


myconfigFile :: String
myconfigFile = "config.csv"

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)


defaultPage :: RoutedServer Url
defaultPage =
  askCnf cnfDefaultPage >>= return . gePage

goto:: Url -> RoutedServer Response
goto url = do
  wp <- askCnf cnfWebpage
  let newPage = wp ++ slashUrlToStr url
  seeOther newPage (toResponse ())

loginHandle ::
  RoutedServer XML -> RoutedServer Response
loginHandle page = msum [
  checkLogin >> page,
  loginPage ] >>= routedOk . toResponse

loadProducts :: RoutedServer [([String], ProductMap)]
loadProducts = do
  config <- ask
  let name = ((csvDirectory ++ "/") ++) . (++ ".csv")
      productFiles = map name $ cnfProducts (cnf config)
      f file txt = parseProduct file txt
  txts <- liftIO (mapM readFile productFiles)
  return $ zipWith f productFiles txts


loadMarkup :: [String] -> IO ContentMap
loadMarkup = fmap (Map.fromList . catMaybes) . mapM go
  where go fn | List.isSuffixOf ".markdown" fn = Just <$> load markdown fn
        go fn | List.isSuffixOf ".markup" fn   = Just <$> load markup fn
        go fn | List.isSuffixOf ".html" fn     = Just <$> load html fn
        go _ = return Nothing

        markdown = writeHtmlString def . readMarkdown def
        markup   = writeHtmlString def . readMediaWiki def
        html     = writeHtmlString def . readHtml def

        load reader fn = do
          c <- readFile $ markupDirectory </> fn
          return $ (T.pack $ takeWhile (/= '.') fn, reader c)

routedGetHandle :: Url -> RoutedServer Response

routedGetHandle p@(WithLang _ LookToken) = loginHandle $ do
  ask >>= getLoginToken >>= (adminTemplate p . show)
routedGetHandle p@(WithLang _ LoginPage) =
  loginHandle (adminTemplate p "Logged in ...")

routedGetHandle (WithLang _ LogoutPage) = do
  expireCookie cretetoken
  config <- ask
  deleteLoginToken config
  dp <- defaultPage
  goto dp

routedGetHandle p@(WithLang _ LoadProd) = loginHandle $ do
  loadProducts >>= adminTemplate p . map productList


routedGetHandle p@(WithLang _ PublishProd) = loginHandle $ do
  config <- ask
  prods <- loadProducts
  let pl = cnfProducts $ cnf config
      (es, ps) = unzip prods
  case (concat es, ps) of
       ([], qs) -> do 
         ask >>= flip setProductListMap (Map.fromList $ zip pl qs)
         adminTemplate p "Neu Produktliste veröffentlicht"
       _ -> adminTemplate p "" -- (productList res)

routedGetHandle p@(WithLang _ LoadMarkup) = loginHandle $ do
  cm <- liftIO $ getDirectoryContents markupDirectory >>= loadMarkup
  config <- ask
  setContentMap config cm
  adminTemplate p "Templates neu geladen ..."

routedGetHandle p@(WithLang _ Restart) = loginHandle $ do
  config <- ask
  liftIO (putMVar (cnfMVar (cnf config)) ())
  adminTemplate p "Schluss aus und vorbei ..."


{-
routedGetHandle (WithLang _ PPRedir) = do
  decodeBody myPolicy
  prod <- look "product"
  config <- ask
  decrProdMap config prod
  goto $ WithLang German Products
-}

routedGetHandle url =
  Index.routed url >>= routedOk . toResponse



(>>!) :: Monad m => m () -> m a -> m a
a >>! b = do { () <- a; b }

site :: Config -> Site Url (Server Response)
site conf =
  setDefault (WithLang German (Page $ T.pack "bla")) $ boomerangSite runRoute urlmap
  where runRoute f url = runReaderT (unRouteT (handle' url) f) conf
        handle' url = incIpMap conf 
                      >>! incUrlMap conf url
                      >>! routedGetHandle url

homePage :: Config -> Server Response
homePage conf = 
  implSite (T.pack $ baseHttpAddr (cnf conf)) (T.pack "") (site conf)
  where baseHttpAddr c = cnfWebpage c ++ show (cnfPort c)

serveFromDir :: FilePath -> FilePath -> Server Response
serveFromDir filepath file =
  serveFile (guessContentTypeM mimeTypes) (filepath ++ "/" ++ file)

handlers :: Config -> Server Response
handlers config = msum [
  dir "css" $ uriRest (serveFromDir cssDirectory),
  dir "img" $ uriRest (serveFromDir imgDirectory),
  homePage config,
  seeOther dp (toResponse ()) ]
  where dp = cnfWebpage (cnf config) ++ "/de/page/"
             ++ cnfDefaultPage (cnf config)

serve ::
  AcidState LoginToken ->
  AcidState StoreState ->
  AcidState ProductListMap ->
  AcidState ContentMap -> IO ()
serve token urlm plmap contentmap = do
  c <- liftIO $ readConfigFile $ csvDirectory ++ "/" ++ myconfigFile
  _ <- forkIO 
       $ simpleHTTP (nullConf {port = cnfPort c})
       $ handlers (Config c token urlm plmap contentmap)
  takeMVar (cnfMVar c)

withAcid ::
  (Typeable st, IsAcidic st) => String -> st -> (AcidState st -> IO a) -> IO a
withAcid file initialState =
  bracket (openLocalStateFrom ("_state" </> file) initialState) 
          createCheckpointAndClose


main :: IO ()
main =
  withAcid "token" initToken $ \token ->
    withAcid "urlmap" initUrlMap $ \urlm ->
      withAcid "products" initProductListMap $ \plmap ->
        withAcid "content" initContentMap $ \content ->
          serve token urlm plmap content
