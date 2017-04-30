{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Proxy
import Data.Time
import Data.Text (Text)

import Servant.API

import Model

type API = "stocks" :> Get '[JSON] [Stock]
      :<|> "trend_sources" :> Get '[JSON] [TrendSource]
      :<|> "news_sources" :> Get '[JSON] [NewsSource]
      :<|> "stock" :> QueryParam "name" Text
                   :> QueryParam "start" UTCTime :> QueryParam "end" UTCTime
                   :> Get '[JSON] StockDataInterval
      :<|> "trends" :> QueryParam "name" Text
                    :> QueryParam "start" UTCTime :> QueryParam "end" UTCTime
                    :> Get '[JSON] TrendDataInterval
      :<|> "news" :> QueryParam "name" Text
                  :> QueryParam "start" UTCTime :> QueryParam "end" UTCTime
                  :> Get '[JSON] [NewsData]
      :<|> "update_stocks" :> QueryParam "name" Text
                           :> QueryParam "since" UTCTime
                           :> Post '[JSON] Bool
api :: Proxy API
api = Proxy

