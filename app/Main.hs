module Main where

import Control.Monad.IO.Class (MonadIO (..))
import Database.Persist (Filter, PersistQueryWrite (deleteWhere), PersistStoreWrite (insertMany))
import Database.Persist.Sql (runMigration)
import Database.Persist.Sqlite (runSqlite)
import Network.Wai.Handler.Warp

import Database qualified as DB
import Server.App (app)
import Types

todos :: [DB.Todo]
todos =
    [ DB.Todo "Learn Haskell" False
    , DB.Todo "Migrate database" False
    , DB.Todo "Be awesome" False
    ]

main :: IO ()
main = do
    let logOpts = LogOptions {logType = Tee "teelog.txt", logThreshhold = Info}
        config = ServerConfig {databaseName = "todo.db", logOptions = logOpts}

    runSqlite (databaseName config) $ do
        -- Migrate DB
        liftIO $ putStrLn "Migrating the database"
        runMigration DB.migrateAll
        liftIO $ putStrLn "Database migration done"

        -- Clear and insert default data
        deleteWhere ([] :: [Filter DB.Todo])
        insertMany todos
        liftIO $ putStrLn "Seeding complete"

    liftIO $ run 3000 (app config)
