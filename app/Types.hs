module Types where

import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Writer (WriterT)
import Data.Text (Text)
import Servant.Server (Handler)

-- Server related

type ServerM = LogT (ReaderT ServerConfig Handler)

data ServerConfig = ServerConfig
    { databaseName :: Text
    , logOptions :: LogOptions
    }

-- Logging

data LogLevel
    = Trace
    | Info
    | Warning
    | Error
    deriving (Eq, Ord)

data Log = Log LogLevel String deriving (Show)

data LogType
    = Print
    | File String
    | Silent
    | Tee String

type LogT a = WriterT [Log] a

data LogOptions = LogOptions
    { logType :: LogType
    , logThreshhold :: LogLevel
    }

instance Show LogLevel where
    show Trace = "[TRCE]"
    show Info = "[INFO]"
    show Warning = "[WARN]"
    show Error = "[ERRO]"
