{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model (module Model.Schema, module Model) where

import Control.Lens hiding ((.=))
import Data.Aeson.Types
import Data.Map.Strict (Map)
import Data.String.Conversions
import Data.Text (Text)
import Data.Time
import GHC.Generics
import qualified Data.Map.Strict as M

import Model.Schema

jsonKeys :: Map String String
jsonKeys = M.fromList
  [
    ("marketDataIntervalTimePeriod", "time_period")
  , ("marketData", "data")
  , ("trendDataIntervalTimePeriod", "time_period")
  , ("trendData", "data")
  ]

data TimePeriod = TimePeriod
  {
    start :: Maybe UTCTime
  , end :: Maybe UTCTime
  , splits :: Int
  } deriving (Show, Eq, Generic, ToJSON)

data MarketDataInterval = MarketDataInterval
  {
    marketDataIntervalTimePeriod :: TimePeriod
  , marketData :: [Double]
  } deriving (Show, Eq, Generic)

instance ToJSON MarketDataInterval where
  toJSON = genericToJSON defaultOptions
    {
      fieldLabelModifier = \k -> M.findWithDefault k k jsonKeys
    }

data TrendDataInterval = TrendDataInterval
  {
    trendDataIntervalTimePeriod :: TimePeriod
  , trendData :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON TrendDataInterval where
  toJSON = genericToJSON defaultOptions
    {
      fieldLabelModifier = \k -> M.findWithDefault k k jsonKeys
    }

instance ToJSON Market where
  toJSON = String . cs . view marketName

instance ToJSON TrendSource where
  toJSON = String . cs . view trendSourceName

instance ToJSON NewsSource where
  toJSON = String . cs . view newsSourceName

instance ToJSON NewsData where
  toJSON news = object
    [
      "date" .= (news ^. newsDataDate)
    , "headline" .= (news ^. newsDataHeadline)
    , "link" .= (news ^. newsDataLink)
    ]

