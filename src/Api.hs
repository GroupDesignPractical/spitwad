{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Proxy
import Data.Time

import Servant.API

import Model

type API = "markets" :> Get '[JSON] [Market]
      :<|> "trend_sources" :> Get '[JSON] [TrendSource]
      :<|> "news_sources" :> Get '[JSON] [NewsSource]
      :<|> "market" :> QueryParam "name" MarketId
                    :> QueryParam "start" UTCTime :> QueryParam "end" UTCTime
                    :> Get '[JSON] MarketDataInterval
      :<|> "trends" :> QueryParam "name" TrendSourceId
                    :> QueryParam "start" UTCTime :> QueryParam "end" UTCTime
                    :> Get '[JSON] TrendDataInterval
      :<|> "news" :> QueryParam "name" NewsSourceId
                  :> QueryParam "start" UTCTime :> QueryParam "end" UTCTime
                  :> Get '[JSON] [NewsData]

api :: Proxy API
api = Proxy

