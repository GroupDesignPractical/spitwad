{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module App (run) where

import Prelude hiding ((.))
import Control.Category
import Control.Concurrent.Async.Lifted
import Control.Concurrent.QSem
import Control.Lens hiding ((<.), like)
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Maybe
import Data.String.Conversions
import Data.Time
import Data.Text (Text)
import qualified Data.Text.IO as T.IO

import Network.Wai.Middleware.Cors
import qualified Network.Wai.Handler.Warp as Warp
import Web.Authenticate.OAuth
import Database.Persist.Sql
import Servant

import Api
import Config
import Model
import Scrape.Facebook
import Scrape.NewsAPI
import Scrape.Quandl
import Scrape.Twitter

type LoggingHandler = ExceptT ServantErr (LoggingT IO)

newtype App a = App
  {
    runApp :: ReaderT Config LoggingHandler a
  } deriving (Functor, Applicative, Monad,
              MonadLogger, MonadReader Config, MonadError ServantErr, MonadIO,
              MonadBase IO)

instance MonadBaseControl IO App where
  type StM App a = Either ServantErr a
  liftBaseWith f = App (liftBaseWith (\g -> f (g . runApp)))
  restoreM st = App (restoreM st)

app :: Config -> Application
app = simpleCors . serve api . withConfig

withConfig :: Config -> Server API
withConfig = flip enter server . convertApp

convertApp :: Config -> App :~> Handler
convertApp cfg = hoistNat (Nat runStdoutLoggingT)
                 . Nat (flip runReaderT cfg . runApp)

run :: Config -> IO ()
run cfg = runStdoutLoggingT . flip runReaderT cfg $ do
  p <- asks port
  pool <- asks connectionPool
  $(logInfo) "Initialising"
  runSqlPool (runMigration migrateAll) pool
  $(logInfo) "Migrations done"
  sc <- runDb $ count ([] :: [Filter Stock])
  when (sc == 0) $ do
    fp <- asks stockBootstrapFilePath
    runReaderT (initialiseDb fp) cfg
  nsc <- runDb $ count ([] :: [Filter NewsSource])
  when (nsc == 0) $ do
    fp <- asks newsSourceBootstrapFilePath
    runReaderT (initialiseDb fp) cfg
  tsc <- runDb $ count ([] :: [Filter TrendSource])
  when (tsc == 0) $ do
    fp <- asks trendSourceBootstrapFilePath
    runReaderT (initialiseDb fp) cfg
  $(logInfo) "Loaded"
  napiKey <- asks newsApiKey
  when (isJust napiKey)
    . $(logInfo) $ "Using News API key " <> cs (fromJust napiKey)
  qapiKey <- asks quandlApiKey
  when (isJust qapiKey)
    . $(logInfo) $ "Using Quandl API key " <> cs (fromJust qapiKey)
  tock <- asks twitterOauthConsumerKey
  when (isJust tock)
    . $(logInfo) $ "Using Twitter OAuth consumer key "
      <> cs (fromJust tock)
  tocs <- asks twitterOauthConsumerSecret
  when (isJust tocs)
    . $(logInfo) $ "Using Twitter OAuth consumer secret "
      <> cs (fromJust tocs)
  tot <- asks twitterOauthToken
  when (isJust tot)
    . $(logInfo) $ "Using Twitter OAuth token " <> cs (fromJust tot)
  tots <- asks twitterOauthTokenSecret
  when (isJust tots)
    . $(logInfo) $ "Using Twitter OAuth token secret "
      <> cs (fromJust tots)
  fbat <- asks facebookAccessToken
  when (isJust fbat)
    . $(logInfo) $ "Using Facebook access token "
      <> cs (fromJust fbat)
  liftIO . Warp.run p $ app cfg

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO a -> m a
runDb q = do
  pool <- asks connectionPool
  liftIO $ runSqlPool q pool

server :: ServerT API App
server = getStocks :<|> getTrendSources :<|> getNewsSources
    :<|> getStockDataInterval :<|> getTrendDataInterval :<|> getNewsInterval
    :<|> updateStockData :<|> updateNewsData :<|> updateTrends
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
        getStockDataInterval :: Maybe Text -- stock_symbol
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
        getTrendDataInterval :: Maybe Text -- trend_source_name
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
        getNewsInterval :: Maybe Text -- news_source_name
                        -> Maybe UTCTime -> Maybe UTCTime -- start, end
                        -> App [NewsData]
        getNewsInterval (Just nsaname) (Just s) (Just e) = do
          selected <- runDb $ selectList
            [NewsDataNews_source_api_name ==. nsaname
           , NewsDataDate >. s, NewsDataDate <. e]
            [Asc NewsDataDate]
          pure $ entityVal <$> selected
        getNewsInterval (Just _) _ _ = throwError
          err400 { errBody = "Bad Request, invalid dates" }
        getNewsInterval _ _ _ = throwError
          err400 { errBody = "Bad Request, invalid news source name" }
        updateStockData :: Maybe Text -> Maybe UTCTime -> App Bool
        updateStockData (Just sym) since = do
          -- TODO: don't retrieve stock data for values already in db
          apiKey <- asks quandlApiKey
          rows <- liftIO $ scrapeStockData (cs sym) since apiKey
          -- TODO: use insertBy and collect failures
          res <- mapM (runDb . insertUnique) rows
          pure . or $ map isJust res
        updateStockData Nothing since = do
          selected <- runDb $ selectList [] []
          let stocks = entityVal <$> selected
              symbols = map (^. stockSymbol) stocks
          res <- mapM (flip updateStockData since . Just) symbols
          pure $ or res
        updateNewsData :: Maybe Text -> App Bool
        updateNewsData (Just aname) = do
          apiKey <- asks newsApiKey
          ft <- asks facebookAccessToken
          Just ns <- runDb $ selectFirst [NewsSourceApi_name ==. aname] []
          let np = cs . filter (/= '/') . uriPath
                $ entityVal ns ^. newsSourceFacebook_page
          rows <- liftIO . runStdoutLoggingT $ scrapeNewsData (cs aname) apiKey
          rows' <- liftIO . runStdoutLoggingT $ populateReacts ft np rows
          -- TODO: use insertBy and collect failures
          res <- mapM (runDb . insertUnique) rows'
          pure . or $ map isJust res
        updateNewsData Nothing = do
          selected <- runDb $ selectList [] []
          let newsSources = entityVal <$> selected
              apiNames = map (^. newsSourceApi_name) newsSources
          n <- asks nworkers
          sem <- liftIO $ newQSem n
          res <- mapConcurrently (\x -> liftIO (waitQSem sem)
                                     >> updateNewsData (Just x)
                                     >>= \y -> liftIO (signalQSem sem)
                                            >> pure y) apiNames
          pure $ or res
        populateReacts :: Maybe Text -> Text -> [NewsData]
                       -> LoggingT IO [NewsData]
        populateReacts ft np nds = do
          reacts <- getReacts ft np $ map (^. newsDataLink) nds
          zipWithM (\nd -> maybe ($(logError) ("\x1b[31mNo reacts found for "
                                    <> nd ^. newsDataHeadline <> "\x1b[0m")
                                    >> pure nd
                                 )
                          (\r ->
                            $(logInfo) ("\x1b[32mGot reacts " <> cs (show r)
                              <> " for " <> nd ^.newsDataHeadline <> "\x1b[0m")
                            >> pure (nd
                                & newsDataFacebook_react_like .~ like r
                                & newsDataFacebook_react_love .~ love r
                                & newsDataFacebook_react_haha .~ haha r
                                & newsDataFacebook_react_wow .~ wow r
                                & newsDataFacebook_react_sad .~ sad r
                                & newsDataFacebook_react_angry .~ angry r)
                          )) nds reacts
        updateTrends :: App Bool
        updateTrends = do
          tock <- asks twitterOauthConsumerKey
          tocs <- asks twitterOauthConsumerSecret
          tot <- asks twitterOauthToken
          tots <- asks twitterOauthTokenSecret
          let tokens = newOAuth
                {
                  oauthConsumerKey = cs $ fromMaybe "" tock
                , oauthConsumerSecret = cs $ fromMaybe "" tocs
                }
              credential = Credential
                [
                  ("oauth_token", cs $ fromMaybe "" tot)
                , ("oauth_token_secret", cs $ fromMaybe "" tots)
                ]
          rows <- liftIO $ scrapeTwitterTrends tokens credential
          _ <- runDb $ insertMany rows
          pure True

initialiseDb :: (MonadLogger m, MonadReader Config m, MonadIO m,
                 MonadBaseControl IO m) => FilePath -> m ()
initialiseDb sqlfp = do
  sql <- liftIO $ T.IO.readFile sqlfp
  $(logInfo) $ "Bootstrapping..." <> cs sqlfp
  -- $(logInfo) $ cs sql
  pool <- asks connectionPool
  runSqlPool (rawExecute sql []) pool
  pure ()

