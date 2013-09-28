

module Crete.Type where

import Control.Monad.Reader

import qualified Data.Map as Map; import Data.Map (Map)
import Data.Acid
import Data.Maybe (mapMaybe)

import Text.ParserCombinators.Parsec (parse)

import Happstack.Server

import Web.Routes

import Crete.Store.StoreTypes
import Crete.Url.Url
import Crete.Utility (trimm)


import qualified Crete.Product.ProductParser as PP



type Server       = ServerPartT IO
type ReaderServer = ReaderT Config Server
type RoutedServer = RouteT Url ReaderServer


liftRouted :: Server a -> RoutedServer a
liftRouted = (lift $) . (lift $)


routedOk :: a -> RoutedServer a
routedOk = liftRouted . ok


data Cnf = Cnf {
  cnfName        :: String,
  cnfStreet      :: String,
  cnfCity        :: String,
  cnfCountry     :: String,
  cnfFax         :: String,
  cnfFon         :: String,
  cnfEmail       :: String,
  cnfWebpage     :: String,
  cnfPort        :: Int,
  cnfDefaultPage :: String,
  cnfProducts    :: String,
  cnfPPAddress   :: String,
  cnfPPBusiness  :: String,
  cnfPPCurrency  :: String,
  cnfPPShipping  :: String } deriving (Show)


parseConfigFile :: FilePath -> String -> Map String String
parseConfigFile file input = 
  case parse (PP.csvFile ',') file input of
       Right res -> Map.fromList $ mapMaybe f res
       Left msg -> error (show msg)
  where f [item, value] = Just (trimm item, trimm value)
        f [] = Nothing
        f xs = error $ "Missing field for at least one config option. Found"
                       ++ show xs ++ " Quitting..."

readConfigFile :: FilePath -> IO Cnf
readConfigFile file = do
  txt <- readFile file
  let m = parseConfigFile file txt
      getItem str = 
        maybe (error $ "Field " ++ str ++ " is not present in " 
                       ++ show file ++ ". Quitting...") id
        . Map.lookup str
  return $
    Cnf {
      cnfName        = getItem "name" m,
      cnfStreet      = getItem "street" m,
      cnfCity        = getItem "city" m,
      cnfCountry     = getItem "country" m,
      cnfEmail       = getItem "email" m,
      cnfWebpage     = getItem "webpage" m,
      cnfFon         = getItem "fon" m,
      cnfFax         = getItem "fax" m,
      cnfDefaultPage = getItem "defaultpage" m,
      cnfPort        = read $ getItem "port" m,
      cnfProducts    = getItem "products" m,
      cnfPPAddress   = getItem "paypal_address" m,
      cnfPPBusiness  = getItem "paypal_business" m,
      cnfPPCurrency  = getItem "paypal_currency_code" m,
      cnfPPShipping  = getItem "paypal_shipping" m }



data Config = Config { cnf :: Cnf,
                       acidLoginToken :: AcidState LoginToken,
                       acidStore :: AcidState StoreState,
                       acidProducts :: AcidState ProductMap,
                       acidContent :: AcidState ContentMap }
