module Logging
    ( Log (..)
    , LogT
    , LogLevel (..)
    , LogType (..)
    , log
    , logError
    , logInfo
    , logTrace
    , logWarning
    , runLogger
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Writer (WriterT (runWriterT), tell)
import Data.Time (UTCTime, getCurrentTime)
import Prelude hiding (log)

import Control.Monad.Trans.Reader (ReaderT, asks)
import Servant (Handler)
import Types

decorate :: Log -> UTCTime -> String
decorate (Log level message) time = show level <> " [" <> show time <> "] " <> message

-- Logging helpers

logTrace :: Monad m => String -> LogT m ()
logTrace = log . Log Trace

logInfo :: Monad m => String -> LogT m ()
logInfo = log . Log Info

logWarning :: Monad m => String -> LogT m ()
logWarning = log . Log Warning

logError :: Monad m => String -> LogT m ()
logError = log . Log Error

log :: Monad m => Log -> LogT m ()
log = tell . return

-- Running different log types

printLog :: Log -> IO ()
printLog l = getCurrentTime >>= putStrLn . decorate l

logToFile :: String -> Log -> IO ()
logToFile filename l = do
    time <- getCurrentTime
    appendFile filename (decorate l time <> "\n")

logTee :: String -> Log -> IO ()
logTee filename l = logToFile filename l >> printLog l

-- Perform logging

performLog :: [Log] -> LogOptions -> IO ()
performLog ls (LogOptions lt threshold) =
    let ls' = filter (\(Log level _) -> level >= threshold) ls
     in case lt of
            File filename -> mapM_ (logToFile filename) ls'
            Print -> mapM_ printLog ls'
            Silent -> return ()
            Tee filename -> mapM_ (logTee filename) ls'

runLogger :: ServerM a -> ReaderT ServerConfig Handler a
runLogger a = do
    (v, l) <- runWriterT a
    logOpts <- asks logOptions
    liftIO $ performLog l logOpts
    return v
