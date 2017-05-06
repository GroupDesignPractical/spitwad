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
import Control.Concurrent
import Control.Concurrent.Async.Lifted
import Control.Exception.Safe hiding (Handler)
import Control.Lens hiding ((<.), like)
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time
import Data.Text (Text)
import qualified Data.Text.IO as T.IO
import System.Random

import Network.HTTP.Types.URI
import Network.Wai.Middleware.Cors
import qualified Network.Wai.Handler.Warp as Warp
import Web.Authenticate.OAuth
import Database.Persist.Sql
import Servant

import Analyse.Sentiment
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
              MonadBase IO, MonadCatch, MonadError ServantErr, MonadIO,
              MonadLogger, MonadReader Config, MonadThrow)

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
    . $(logInfo) $ "Using Twitter OAuth consumer key " <> cs (fromJust tock)
  tocs <- asks twitterOauthConsumerSecret
  when (isJust tocs)
    . $(logInfo) $ "Using Twitter OAuth consumer secret " <> cs (fromJust tocs)
  tot <- asks twitterOauthToken
  when (isJust tot)
    . $(logInfo) $ "Using Twitter OAuth token " <> cs (fromJust tot)
  tots <- asks twitterOauthTokenSecret
  when (isJust tots)
    . $(logInfo) $ "Using Twitter OAuth token secret " <> cs (fromJust tots)
  fbat <- asks facebookAccessToken
  when (isJust fbat)
    . $(logInfo) $ "Using Facebook access token " <> cs (fromJust fbat)
  iapiKey <- asks indicoApiKey
  when (isJust iapiKey)
    . $(logInfo) $ "Using Indico API key " <> cs (fromJust iapiKey)
  liftIO . Warp.run p $ app cfg

runDb :: (MonadCatch m, MonadIO m, MonadLogger m, MonadReader Config m)
      => SqlPersistT IO a -> m a
runDb q = catchAny (do
  pool <- asks connectionPool
  liftIO $ runSqlPool q pool)
  (\e -> $(logError) (cs $ show e)
    >> liftIO (randomRIO (250000, 3000000)) -- retry after (0.25, 3) seconds
    >>= liftIO . threadDelay
    >> runDb q)

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
            [Asc TrendDataDate, Desc TrendDataVolume]
          let points = entityVal <$> selected
          pure TrendDataInterval
            {
              trendDataIntervalTimePeriod = TimePeriod
                {
                  start = points ^? _head . trendDataDate
                , end = points ^? _last . trendDataDate
                , splits = length selected
                }
            , trendData = mostPopular points
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
          $(logInfo) $ "\x1b[35mUpdating " <> aname <> " \x1b[0m"
          apiKey <- asks newsApiKey
          ft <- asks facebookAccessToken
          Just ns <- runDb $ selectFirst [NewsSourceApi_name ==. aname] []
          let np = cs . filter (/= '/') . uriPath
                $ entityVal ns ^. newsSourceFacebook_page
          rows <- scrapeNewsData (cs aname) apiKey
          rows' <- populateReacts ft np rows
          -- TODO: use insertBy and collect failures
          res <- mapM (runDb . insertUnique) rows'
          $(logInfo) $ "\x1b[35m" <> aname <> " update done \x1b[0m"
          pure . or $ map isJust res
        updateNewsData Nothing = do
          $(logInfo) "\x1b[33mUpdating all news\x1b[0m"
          selected <- runDb $ selectList [] []
          let newsSources = entityVal <$> selected
              apiNames = map (^. newsSourceApi_name) newsSources
          n <- asks nworkers
          sem <- liftIO $ newQSem n
          res <- mapConcurrently (\x -> liftIO (waitQSem sem)
                                     >> updateNewsData (Just x)
                                     >>= \y -> liftIO (signalQSem sem)
                                            >> pure y) apiNames
          $(logInfo) "\x1b[33mNews update done\x1b[0m"
          pure $ or res
        populateReacts :: (MonadBaseControl IO m, MonadCatch m, MonadIO m,
                           MonadLogger m)
                       => Maybe Text -> Text -> [NewsData] -> m [NewsData]
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
          $(logInfo) "\x1b[35mUpdating trends\x1b[0m"
          tock <- asks twitterOauthConsumerKey
          tocs <- asks twitterOauthConsumerSecret
          tot <- asks twitterOauthToken
          tots <- asks twitterOauthTokenSecret
          apiKey <- asks indicoApiKey
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
          rows' <- mapConcurrently (tweetSentiment tokens credential apiKey)
                                   rows
          _ <- runDb $ insertMany rows'
          $(logInfo) "\x1b[35mTrend update done\x1b[0m"
          pure True
        tweetSentiment :: (MonadIO m, MonadLogger m)
                       => OAuth -> Credential -> Maybe Text
                       -> TrendData -> m TrendData
        tweetSentiment tokens credential apiKey td = do
          let trend = td ^. trendDataDatum
          $(logInfo) $ "\x1b[34mGetting tweets for " <> trend <> "\x1b[0m"
          tweets <- liftIO . getTweets tokens credential . cs . urlEncode True
                      $ cs trend
          if null tweets
            then do $(logError) $ "\x1b[31mNo tweets found for " <> trend
                                    <> "\x1b[0m"
                    pure td
            else do -- $(logInfo) $ "\x1b[34mGot tweets " <> cs (show tweets)
                                    -- <> "\x1b[0m"
                    sentiments <- liftIO $ analyseSentiments apiKey tweets
                    let average = sum sentiments /
                                    fromIntegral (length sentiments)
                    $(logInfo) $ "\x1b[34mGot sentiment " <> cs (show average)
                                    <> " for " <> trend <> "\x1b[0m"
                    pure $ td & trendDataSentiment .~ average

mostPopular :: [TrendData] -> [TrendData]
mostPopular = nubBy (\td td' -> td ^. trendDataDate == td' ^. trendDataDate)

initialiseDb :: (MonadLogger m, MonadReader Config m, MonadIO m,
                 MonadBaseControl IO m) => FilePath -> m ()
initialiseDb sqlfp = do
  sql <- liftIO $ T.IO.readFile sqlfp
  $(logInfo) $ "Bootstrapping..." <> cs sqlfp
  -- $(logInfo) $ cs sql
  pool <- asks connectionPool
  runSqlPool (rawExecute sql []) pool
  pure ()

