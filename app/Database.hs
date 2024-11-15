{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database where

import Data.Aeson (FromJSON, ToJSON)
import Database.Persist.TH
import GHC.Generics

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
Todo
    title String
    completed Bool
    deriving Show Generic
|]

instance FromJSON Todo
instance ToJSON Todo
