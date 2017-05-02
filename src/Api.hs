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
      :<|> "stock" :> QueryParam "symbol" Text
                   :> QueryParam "start" UTCTime :> QueryParam "end" UTCTime
                   :> Get '[JSON] StockDataInterval
      :<|> "trends" :> QueryParam "source" Text
                    :> QueryParam "start" UTCTime :> QueryParam "end" UTCTime
                    :> Get '[JSON] TrendDataInterval
      :<|> "news" :> QueryParam "source" Text
                  :> QueryParam "start" UTCTime :> QueryParam "end" UTCTime
                  :> Get '[JSON] [NewsData]
      :<|> "update_stocks" :> QueryParam "symbol" Text
                           :> QueryParam "since" UTCTime
                           :> Post '[JSON] Bool
      :<|> "update_news" :> QueryParam "source" Text
                         :> Post '[JSON] Bool
api :: Proxy API
api = Proxy

