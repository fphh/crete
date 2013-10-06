{-# LANGUAGE DeriveDataTypeable #-}

module Crete.Store.StoreTypes where

import Data.Map (Map)
import Data.Data (Typeable, Data)
import Data.Text (Text)

import Crete.Url.Url


type IP = String

type IpMap = Map IP Int

type UrlMap = Map Url Int

data StoreState = StoreState {
  urlMap :: UrlMap,
  ipMap :: IpMap } deriving (Eq, Show, Read, Ord, Data, Typeable)


newtype LoginToken = LoginToken { unToken :: Maybe String }
  deriving (Eq, Show, Read, Ord, Data, Typeable)


data Product = Product {
     productQuantity :: Int,
     productDescription :: String,
     productPicture :: String,
     productUnit :: String,
     productPrice :: Double } deriving (Eq, Show, Read, Ord, Data, Typeable)

type ProductName = String
type ListName = String

type ProductMap = Map ProductName Product

type ProductListMap = Map ListName ProductMap


type PageName = Text


type ContentMap = Map PageName String