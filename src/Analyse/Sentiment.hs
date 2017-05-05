{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Analyse.Sentiment (analyseSentiments) where

import Data.Aeson
import Data.Aeson.Types
import Data.String.Conversions
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Simple

newtype IndicoQuery = IndicoQuery
  {
    _data :: [Text]
  } deriving (Generic)

newtype IndicoResults = IndicoResults
  {
    results :: [Double]
  } deriving (Show, Generic, FromJSON)

instance ToJSON IndicoQuery where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

analyseSentiments :: Maybe Text -> [Text] -> IO [Double]
analyseSentiments indicoKey input =
  parseRequest "POST https://apiv2.indico.io/sentimenthq/batch"
  >>= httpJSON . setRequestBodyJSON (IndicoQuery input)
               . addRequestHeader "X-ApiKey" (maybe "" cs indicoKey)
  >>= pure . results . getResponseBody
