{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Crete.Store.Store where


import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative

import qualified Data.Map as Map
import Data.Acid            ( Query, Update, makeAcidic )
import Data.Acid.Advanced   ( query', update' )
--import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy )
import Data.ByteString.Char8 (unpack)

import Data.Data ()
import Happstack.Server


import Crete.Url.Url
import Crete.Type
import Crete.Store.StoreTypes


xrealip :: String
xrealip = "x-real-ip"

$(deriveSafeCopy 0 'base ''StoreState)
$(deriveSafeCopy 0 'base ''Url)
$(deriveSafeCopy 0 'base ''Sitemap)
$(deriveSafeCopy 0 'base ''Lang)


initUrlMap :: StoreState
initUrlMap = StoreState Map.empty Map.empty

incUrlMap' :: Url -> Update StoreState ()
incUrlMap' url = do
  m <- get
  let newUrlMap = Map.insertWith (+) url 1 (urlMap m)
  put $ m { urlMap = newUrlMap }

lookUrlMap' :: Query StoreState UrlMap
lookUrlMap' = urlMap <$> ask


incIpMap' :: IP -> Update StoreState ()
incIpMap' ip = do
  m <- get
  let newIpMap = Map.insertWith (+) ip 1 (ipMap m)
  put $ m { ipMap = newIpMap }


lookIpMap' :: Query StoreState IpMap
lookIpMap' = ipMap <$> ask

$(makeAcidic ''StoreState ['incUrlMap', 'lookUrlMap', 'incIpMap', 'lookIpMap'])

incUrlMap :: (ServerMonad m, MonadIO m) => Config -> Url -> m ()
incUrlMap s url = update' (acidStore s) (IncUrlMap' url)

lookUrlMap :: (MonadIO m) => Config -> m UrlMap
lookUrlMap s = query' (acidStore s) LookUrlMap'


incIpMap :: (ServerMonad m, MonadIO m, Functor m) => Config -> m ()
incIpMap s = do
  rq <- getHeader xrealip <$> askRq
  let st = acidStore s
  case rq of
       Just ip -> update' st (IncIpMap' $ unpack ip)
       Nothing -> update' st (IncIpMap' "unknown IP")


lookIpMap :: (MonadIO m) => Config -> m IpMap
lookIpMap (Config _ _ acid _ _) = query' acid LookIpMap'


----------------------------------------------------------------

initToken :: LoginToken
initToken = LoginToken Nothing

$(deriveSafeCopy 0 'base ''LoginToken)

setLoginToken' :: LoginToken -> Update LoginToken ()
setLoginToken' = put

getLoginToken' :: Query LoginToken LoginToken
getLoginToken' = ask


$(makeAcidic ''LoginToken ['setLoginToken', 'getLoginToken'])

setLoginToken :: (ServerMonad m, MonadIO m) => Config -> String -> m ()
setLoginToken t token =
  update' (acidLoginToken t) (SetLoginToken' (LoginToken (Just token)))

getLoginToken :: (ServerMonad m, MonadIO m) => Config -> m LoginToken
getLoginToken t = query' (acidLoginToken t) GetLoginToken'

deleteLoginToken :: (ServerMonad m, MonadIO m) => Config -> m ()
deleteLoginToken t =
  update' (acidLoginToken t) (SetLoginToken' (LoginToken Nothing))


----------------------------------------------------------------

initProductListMap :: ProductListMap
initProductListMap = Map.empty

$(deriveSafeCopy 0 'base ''Product)

setProductListMap' :: ProductListMap -> Update ProductListMap ()
setProductListMap' = put

getProductListMap' :: Query ProductListMap ProductListMap
getProductListMap' = ask

decrProductListMap' :: ListName -> ProductName -> Update ProductListMap ()
decrProductListMap' ln pn = do
  m <- get
  let decr p = p { productQuantity = productQuantity p - 1 }
      f x = Map.insert ln (Map.adjust decr pn x) m
  put $ maybe (error $ "decrProductListMap': Product list \"" ++ ln ++ "\" not found")
              f (Map.lookup ln m)

$(makeAcidic ''ProductListMap
             ['getProductListMap', 'setProductListMap', 'decrProductListMap'])


setProductListMap :: (ServerMonad m, MonadIO m) => Config -> ProductListMap -> m ()
setProductListMap p pm = update' (acidProducts p) (SetProductListMap' pm)

getProductListMap :: (ServerMonad m, MonadIO m) => Config -> m ProductListMap
getProductListMap p = query' (acidProducts p) GetProductListMap'

decrProductListMap ::
  (ServerMonad m, MonadIO m) => Config -> ListName -> ProductName -> m ()
decrProductListMap p ln pn = update' (acidProducts p) (DecrProductListMap' ln pn)


----------------------------------------------------------------

initContentMap :: ContentMap
initContentMap = Map.empty


setContentMap' :: ContentMap -> Update ContentMap ()
setContentMap' = put

getContentMap' :: Query ContentMap ContentMap
getContentMap' = ask

$(makeAcidic ''ContentMap ['getContentMap', 'setContentMap'])

setContentMap :: (ServerMonad m, MonadIO m) => Config -> ContentMap -> m ()
setContentMap c cm = update' (acidContent c) (SetContentMap' cm)

getContentMap :: (ServerMonad m, MonadIO m) => Config -> m ContentMap
getContentMap c = query' (acidContent c) GetContentMap'
