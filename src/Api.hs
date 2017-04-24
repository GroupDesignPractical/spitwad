{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Proxy
import Data.Time

import Servant.API

import Model

type API = "stocks" :> Get '[JSON] [Stock]
      :<|> "trend_sources" :> Get '[JSON] [TrendSource]
      :<|> "news_sources" :> Get '[JSON] [NewsSource]
      :<|> "stock" :> QueryParam "name" StockId
                    :> QueryParam "start" UTCTime :> QueryParam "end" UTCTime
                    :> Get '[JSON] StockDataInterval
      :<|> "trends" :> QueryParam "name" TrendSourceId
                    :> QueryParam "start" UTCTime :> QueryParam "end" UTCTime
                    :> Get '[JSON] TrendDataInterval
      :<|> "news" :> QueryParam "name" NewsSourceId
                  :> QueryParam "start" UTCTime :> QueryParam "end" UTCTime
                  :> Get '[JSON] [NewsData]

api :: Proxy API
api = Proxy

