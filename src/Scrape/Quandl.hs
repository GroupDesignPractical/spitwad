{-# LANGUAGE OverloadedStrings #-}

module Scrape.Quandl (scrapeStockData) where

import Data.Aeson
import Data.Aeson.Types
import Data.Monoid
import Data.String.Conversions
import Data.Text (Text)
import Data.Time
import Network.HTTP.Simple
import qualified Data.Vector as V

import Model.Schema

data QuandlStock = QuandlStock
  {
    symbol :: String
  , dateprice :: QuandlStockData
  } deriving (Show)

type QuandlStockPoint = (String, Double)

newtype QuandlStockData = QuandlStockData
  {
    getQuandlStockData :: [QuandlStockPoint]
  } deriving (Show)

extract :: Value -> Parser QuandlStockData
extract = withArray "datapoints" $ \a -> QuandlStockData <$>
  mapM extractPoint (V.toList a)

extractPoint :: Value -> Parser QuandlStockPoint
extractPoint = withArray "datapoint" $ \arr -> do
  date <- parseJSON $ V.head arr
  price <- parseJSON $ V.head (V.tail arr)
  return (date, price)

instance FromJSON QuandlStockData where
  parseJSON = withObject "response" $ \o -> do
    datasetData <- o .: "dataset_data"
    datasetData .: "data" >>= extract

getQuandlJSON :: String -> Maybe UTCTime -> Maybe Text -> IO QuandlStockData
getQuandlJSON sym date apiKey = parseRequest
  ("GET https://www.quandl.com/api/v3/datasets/LSE/"
    <> sym <> "/data.json?column_index=1"
    <> maybe "" (("&start_date=" <>) . formatTime defaultTimeLocale "%F") date
    <> maybe "" (("&api_key=" <>) . cs) apiKey
  )
  >>= httpJSON
  >>= (pure . getResponseBody)

quandlStockData :: QuandlStock -> [StockData]
quandlStockData qs = map (uncurry $ StockData (cs $ symbol qs) . unsafeParseYMD)
  . getQuandlStockData $ dateprice qs

scrapeStockData :: String -> Maybe UTCTime -> Maybe Text -> IO [StockData]
scrapeStockData sym date apiKey = getQuandlJSON sym date apiKey
  >>= pure . quandlStockData . QuandlStock sym

unsafeParseYMD :: String -> UTCTime
unsafeParseYMD = parseTimeOrError False defaultTimeLocale "%F"
