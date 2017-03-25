{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
Market
  name Text
  Primary name
MarketData
  marketName MarketId
  date UTCTime default=CURRENT_TIMESTAMP
  datum Double
  Primary marketName date
  deriving Show Eq
TrendSource
  name Text
  Primary name
TrendData
  trendSourceName TrendSourceId
  date UTCTime default=CURRENT_TIMESTAMP
  datum Text
  Primary trendSourceName date
  deriving Show Eq
NewsSource
  name Text
  Primary name
NewsData
  newsSourceName NewsSourceId
  date UTCTime default=CURRENT_TIMESTAMP
  headline Text
  link URI
  Primary newsSourceName date
  deriving Show Eq
|]

