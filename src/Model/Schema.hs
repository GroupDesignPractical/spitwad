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
StockData json
  stock_symbol StockId
  date UTCTime default=CURRENT_TIMESTAMP
  datum Double
  Primary stock_symbol date
  deriving Show Eq
TrendSource
  name Text
  Primary name
TrendData json
  trend_source_name TrendSourceId
  date UTCTime default=CURRENT_TIMESTAMP
  datum Text
  sentiment Double
  volume Int
  Primary trend_source_name date
  deriving Show Eq
NewsSource json
  name Text
  facebook_page URI
  Primary name
NewsData
  news_source_name NewsSourceId
  date UTCTime default=CURRENT_TIMESTAMP
  headline Text
  link URI
  facebook_react_like Int
  facebook_react_love Int
  facebook_react_haha Int
  facebook_react_wow Int
  facebook_react_sad Int
  facebook_react_angry Int
  Primary news_source_name date
  deriving Show Eq
|]

