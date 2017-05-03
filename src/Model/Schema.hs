{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Schema where

import Data.Text
import Data.Time
import Database.Persist.TH
import Network.URI

import Model.URI()

share [mkPersist sqlSettings {mpsGenerateLenses = True},
       mkMigrate "migrateAll"] [persistLowerCase|
Stock json
  symbol Text
  name Text
  market_cap_group Int
  icb_supersector Text
  icb_industry Text
  Primary symbol
  UniqueStock symbol
StockData json
  stock_symbol Text
  date UTCTime default=CURRENT_TIMESTAMP
  datum Double
  Primary stock_symbol date
  Foreign Stock fkstock stock_symbol
  UniqueStockData stock_symbol date
  deriving Show Eq
TrendSource
  name Text
  Primary name
  UniqueTrendSource name
TrendData json
  trend_source_name Text
  date UTCTime default=CURRENT_TIMESTAMP
  datum Text
  sentiment Double
  volume Int
  Primary trend_source_name date datum
  Foreign TrendSource fktrendsource trend_source_name
  UniqueTrendData trend_source_name date datum
  deriving Show Eq
NewsSource json
  api_name Text
  name Text
  facebook_page URI
  Primary api_name
  UniqueNewsSource api_name
NewsData
  news_source_api_name Text
  date UTCTime default=CURRENT_TIMESTAMP
  headline Text
  description Text
  link URI
  facebook_react_like Int
  facebook_react_love Int
  facebook_react_haha Int
  facebook_react_wow Int
  facebook_react_sad Int
  facebook_react_angry Int
  Primary news_source_api_name date
  Foreign NewsSource fknewssource news_source_api_name
  UniqueNewsData news_source_api_name date
  deriving Show Eq
|]

