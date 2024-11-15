module Server.Endpoints
    ( getTodos
    , getTodo
    , createTodo
    , updateTodo
    , deleteTodo
    , getCompletedTodos
    , completeTodo
    ) where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (asks, local, ask)
import Database.Persist
    ( Entity (entityVal)
    , PersistEntity (Key)
    , PersistStoreRead (get)
    , PersistStoreWrite (delete, insert, replace, update)
    , selectList
    , (=.)
    , (==.)
    )
import Database.Persist.Sql (SqlPersistM)
import Database.Persist.Sqlite (runSqlite)
import Servant

import Database (Todo)
import Database qualified as DB
import Logging
import Types

runQuery :: SqlPersistM a -> ServerM a
runQuery query = lift (asks databaseName) >>= liftIO . flip runSqlite query

getTodos :: ServerM [Todo]
getTodos = do
    x <- map entityVal <$> runQuery (selectList @DB.Todo [] [])
    if null x
        then logWarning "No todos found"
        else logInfo $ "Grabbed " <> show (length x) <> " todos from database"
    return x

-- Does not trigger logging since it throws
getTodo :: Key Todo -> ServerM Todo
getTodo = (runQuery . get) >=> maybe (throwError err404) return

createTodo :: Todo -> ServerM (Key Todo)
createTodo = runQuery . insert

updateTodo :: Key Todo -> Todo -> ServerM NoContent
updateTodo key todo = do
    runQuery $ replace key todo
    return NoContent

deleteTodo :: Key Todo -> ServerM NoContent
deleteTodo todoId = do
    runQuery $ delete todoId
    logInfo $ "Deleted todo with id " <> show todoId
    lift $ local id ask
    return NoContent

getCompletedTodos :: ServerM [Todo]
getCompletedTodos = map entityVal <$> runQuery (selectList [DB.TodoCompleted ==. True] [])

completeTodo :: Key Todo -> ServerM NoContent
completeTodo key = runQuery $ update key [DB.TodoCompleted =. True] >> return NoContent
