{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module App (app, run) where

import Control.Lens hiding ((<.))
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Time

import Network.Wai.Middleware.Cors
import qualified Network.Wai.Handler.Warp as Warp
import Database.Persist.Sqlite
import Servant

import Api
import Config
import Model

newtype App a = App
  {
    runApp :: ReaderT Config Handler a
  } deriving (Functor, Applicative, Monad,
              MonadReader Config, MonadError ServantErr, MonadIO)

app :: Config -> Application
app cfg = simpleCors (serve api $ withConfig cfg)

withConfig :: Config -> Server API
withConfig cfg = enter (convertApp cfg) server

convertApp :: Config -> App :~> Handler
convertApp cfg = Nat (flip runReaderT cfg . runApp)

run :: Config -> IO ()
run cfg = flip runReaderT cfg $ do
  p <- asks port
  fp <- asks connectionString
  runSqlite fp $ runMigration migrateAll
  -- runReaderT initialiseDb cfg
  liftIO . Warp.run p $ app cfg

runDb :: (MonadReader Config m, MonadIO m)
      => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runDb q = do
  fp <- asks connectionString
  liftIO $ runSqlite fp q

server :: ServerT API App
server = getStocks :<|> getTrendSources :<|> getNewsSources
    :<|> getStockDataInterval :<|> getTrendDataInterval :<|> getNewsInterval
  where getStocks :: App [Stock]
        getStocks = do
          stocks <- runDb $ selectList [] []
          pure $ entityVal <$> stocks
        getTrendSources :: App [TrendSource]
        getTrendSources = do
          trendSources <- runDb $ selectList [] []
          pure $ entityVal <$> trendSources
        getNewsSources :: App [NewsSource]
        getNewsSources = do
          newsSources <- runDb $ selectList [] []
          pure $ entityVal <$> newsSources
        getStockDataInterval :: Maybe StockId -- stock_symbol
                              -> Maybe UTCTime -> Maybe UTCTime -- start, end
                              -> App StockDataInterval
        getStockDataInterval (Just ssym) (Just s) (Just e) = do
          selected <- runDb $ selectList
            [StockDataStock_symbol ==. ssym
           , StockDataDate >. s, StockDataDate <. e]
            [Asc StockDataDate]
          let points = entityVal <$> selected
          pure StockDataInterval
            {
              stockDataIntervalTimePeriod = TimePeriod
                {
                  start = points ^? _head . stockDataDate
                , end = points ^? _last . stockDataDate
                , splits = length selected
                }
            , stockData = points
            }
        getStockDataInterval (Just _) _ _ = throwError
          err400 { errBody = "Bad Request, invalid dates" }
        getStockDataInterval _ _ _ = throwError
          err400 { errBody = "Bad Request, invalid stock name" }
        getTrendDataInterval :: Maybe TrendSourceId -- trend_source_name
                             -> Maybe UTCTime -> Maybe UTCTime -- start, end
                             -> App TrendDataInterval
        getTrendDataInterval (Just tsname) (Just s) (Just e) = do
          selected <- runDb $ selectList
            [TrendDataTrend_source_name ==. tsname
           , TrendDataDate >. s, TrendDataDate <. e]
            [Asc TrendDataDate]
          let points = entityVal <$> selected
          pure TrendDataInterval
            {
              trendDataIntervalTimePeriod = TimePeriod
                {
                  start = points ^? _head . trendDataDate
                , end = points ^? _last . trendDataDate
                , splits = length selected
                }
            , trendData = points
            }
        getTrendDataInterval (Just _) _ _ = throwError
          err400 { errBody = "Bad Request, invalid dates" }
        getTrendDataInterval _ _ _ = throwError
          err400 { errBody = "Bad Request, invalid trend source name" }
        getNewsInterval :: Maybe NewsSourceId -- news_source_name
                        -> Maybe UTCTime -> Maybe UTCTime -- start, end
                        -> App [NewsData]
        getNewsInterval (Just nsname) (Just s) (Just e) = do
          selected <- runDb $ selectList
            [NewsDataNews_source_name ==. nsname
           , NewsDataDate >. s, NewsDataDate <. e]
            [Asc NewsDataDate]
          pure $ entityVal <$> selected
        getNewsInterval (Just _) _ _ = throwError
          err400 { errBody = "Bad Request, invalid dates" }
        getNewsInterval _ _ _ = throwError
          err400 { errBody = "Bad Request, invalid news source name" }

-- initialiseDb :: (MonadReader Config m, MonadIO m) => m ()
-- initialiseDb = do
--   return ()

