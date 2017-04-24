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
    ("stockDataIntervalTimePeriod", "time_period")
  , ("stockData", "data")
  , ("trendDataIntervalTimePeriod", "time_period")
  , ("trendData", "data")
  ]

data TimePeriod = TimePeriod
  {
    start :: Maybe UTCTime
  , end :: Maybe UTCTime
  , splits :: Int
  } deriving (Show, Eq, Generic, ToJSON)

data StockDataInterval = StockDataInterval
  {
    stockDataIntervalTimePeriod :: TimePeriod
  , stockData :: [StockData]
  } deriving (Show, Eq, Generic)

instance ToJSON StockDataInterval where
  toJSON = genericToJSON defaultOptions
    {
      fieldLabelModifier = \k -> M.findWithDefault k k jsonKeys
    }

data TrendDataInterval = TrendDataInterval
  {
    trendDataIntervalTimePeriod :: TimePeriod
  , trendData :: [TrendData]
  } deriving (Show, Eq, Generic)

instance ToJSON TrendDataInterval where
  toJSON = genericToJSON defaultOptions
    {
      fieldLabelModifier = \k -> M.findWithDefault k k jsonKeys
    }

instance ToJSON TrendSource where
  toJSON = String . cs . view trendSourceName

instance ToJSON NewsData where
  toJSON news = object
    [
      "date" .= (news ^. newsDataDate)
    , "headline" .= (news ^. newsDataHeadline)
    , "link" .= (news ^. newsDataLink)
    , "facebook_reacts" .= FacebookReacts
        (news ^. newsDataFacebook_react_like)
        (news ^. newsDataFacebook_react_love)
        (news ^. newsDataFacebook_react_haha)
        (news ^. newsDataFacebook_react_wow)
        (news ^. newsDataFacebook_react_sad)
        (news ^. newsDataFacebook_react_angry)
    ]

data FacebookReacts = FacebookReacts
  {
    like :: Int
  , love :: Int
  , haha :: Int
  , wow :: Int
  , sad :: Int
  , angry :: Int
  } deriving (Show, Eq, Generic, ToJSON)
