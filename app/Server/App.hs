module Server.App where

import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Database.Persist (PersistEntity (Key))
import Servant

import Database (Todo)
import Logging (runLogger)
import Server.Endpoints
import Types

type TodoAPI =
    "todos" :> Get '[JSON] [Todo]
        :<|> "todos" :> Capture "id" (Key Todo) :> Get '[JSON] Todo
        :<|> "todos" :> ReqBody '[JSON] Todo :> Post '[JSON] (Key Todo)
        :<|> "todos" :> Capture "id" (Key Todo) :> ReqBody '[JSON] Todo :> Put '[JSON] NoContent
        :<|> "todos" :> Capture "id" (Key Todo) :> Delete '[JSON] NoContent
        :<|> "todos" :> Capture "id" (Key Todo) :> Put '[JSON] NoContent
        :<|> "todos" :> "completed" :> Get '[JSON] [Todo]

server :: ServerT TodoAPI ServerM
server =
    getTodos
        :<|> getTodo
        :<|> createTodo
        :<|> updateTodo
        :<|> deleteTodo
        :<|> completeTodo
        :<|> getCompletedTodos

app :: ServerConfig -> Application
app config =
    let apiProxy = Proxy :: Proxy TodoAPI
     in serve apiProxy (hoistServer apiProxy (runServer config) server)

runServer :: ServerConfig -> ServerM a -> Handler a
runServer config = flip runReaderT config . runLogger
