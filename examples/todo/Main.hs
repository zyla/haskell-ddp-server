{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Network.WebSockets as WS
import Network.Wai.Handler.WebSockets as WS
import Data.Aeson
import Network.DDP.Protocol
import Network.DDP.Server
import Network.DDP.Server.WebSocket
import Web.Scotty
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

main :: IO ()
main = do
  config <- mkConfig
  server <- newServer config
  scotty 3009 $ do
    get "/" $ do setHeader "Content-Type" "text/html"; file "./index.html"
    get "/bundle.js" $ do setHeader "Content-Type" "text/javascript"; file "./bundle.js"
    get "/index.js" $ do setHeader "Content-Type" "text/javascript"; file "./index.js"
    middleware $ WS.websocketsOr WS.defaultConnectionOptions (toWebSocketServer server)

mkConfig = do
  nextTodoId <- newTVarIO (1 :: Int)
  todosVar <- newTVarIO []

  nextSubIdVar <- newTVarIO (1 :: Int)
  subsVar <- newTVarIO HM.empty
  
  let
    connect _ = return ()

    callMethod _ "addTodo" [String todo] = do
      putStrLn $ "addTodo " ++ show todo

      (id, subs) <- atomically $ do
        id <- readTVar nextTodoId
        writeTVar nextTodoId (succ id)
        modifyTVar todosVar (++[(id, todo)])
        subs <- readTVar subsVar
        return (id, subs)

      forM_ subs $ \onEvent ->
        onEvent (E_Added $ Added "todos" (tshow id) (object ["todo" .= todo]))

      return (Right (object []))

    callMethod _ _ _ = do
      return (Right (object []))

    subscribe _ "todos" _ onEvent = do
      putStrLn $ "subscribe todos"

      (subId, todos) <- atomically $ do
        todos <- readTVar todosVar
        subId <- readTVar nextSubIdVar
        writeTVar nextSubIdVar (succ subId)
        modifyTVar subsVar (HM.insert subId onEvent)
        return (subId, todos)

      forM_ todos $ \(id, todo) ->
        onEvent (E_Added $ Added "todos" (tshow id) (object ["todo" .= todo]))

      let unsubscribe = atomically $ modifyTVar subsVar (HM.delete subId)

      return unsubscribe

    subscribe _ name _ _ = do
      putStrLn $ "subscribe " ++ show name
      return (return ())

  return (Config connect callMethod subscribe)

tshow = T.pack . show
