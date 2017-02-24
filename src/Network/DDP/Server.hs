module Network.DDP.Server where

import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import Network.DDP.Protocol hiding (Connected)
import qualified Network.DDP.Protocol as Protocol
import Control.Concurrent
import Control.Concurrent.STM

type Unsubscribe = IO ()

data DataEvent =
    E_Added Added
  | E_Changed Changed
  | E_Removed Removed

data Config s = Config
  { connect :: ConnectionInfo -> IO s
  , callMethod :: s -> Text -> [Value] -> IO (Either Error Value)
  , subscribe :: s -> Text -> [Value] -> (DataEvent -> IO ()) -> IO Unsubscribe
  }

data Server s = Server { server_config :: Config s }

newServer :: Config s -> IO (Server s)
newServer = return . Server

data ConnectionInfo = ConnectionInfo

data Connection s = Connection
  { connection_config :: Config s
  , connection_state :: TVar (ConnectionState s)
  , connection_sendMessage :: ServerMessage -> IO ()
  }

data ConnectionState s =
    New
  | Connected
      { subs :: TVar (HM.HashMap SubId Unsubscribe)
      , appState :: s }

newConnection :: Server s -> (ServerMessage -> IO ()) ->  IO (Connection s)
newConnection server sendMessage  = do
  state <- newTVarIO New
  return Connection
    { connection_config = server_config server
    , connection_state = state
    , connection_sendMessage = sendMessage
    }

pass :: Applicative f => f ()
pass = pure ()

processMessage :: Connection s -> ClientMessage -> IO ()
processMessage (Connection Config{callMethod,subscribe,connect} stateVar sendMessage) msg = do
  -- FIXME: RACE CONDITION: Two threads may C_Connect at the same time.
  state <- atomically $ readTVar stateVar
  case state of
    New -> case msg of
      C_Connect{} -> do
        -- TODO check versions
        subsVar <- newTVarIO HM.empty
        appState <- connect ConnectionInfo
        sendMessage $ S_Connected $ Protocol.Connected $ SessionId ""
        atomically $ writeTVar stateVar $ Connected subsVar appState

      _ ->
        -- TODO: return "not connected" error
        pass

    Connected subsVar appState -> case msg of

      C_Connect{} ->
        -- TODO error: already connected
        pass

      C_Ping (Ping id) ->
        sendMessage (S_Pong (Pong id))

      C_Pong {} ->
        -- Timeouts should be handled during receive,
        -- so 'pong' serves only as a dummy packet
        pass

      C_Sub Sub{sub_id,sub_name,sub_params} -> do
        let onEvent event = sendMessage (fromDataEvent event)
            fromDataEvent = \case
              E_Added   val -> S_Added val
              E_Changed val -> S_Changed val
              E_Removed val -> S_Removed val

        unsub <- subscribe appState sub_name sub_params onEvent
        -- FIXME doesn't handle duplicate subId case
        atomically $ modifyTVar subsVar $ HM.insert sub_id unsub
        sendMessage $ S_Ready $ Ready [sub_id]

      C_Unsub Unsub{unsub_id} -> do

        m_unsubscribe <- atomically $ do
          subs <- readTVar subsVar
          let m_unsubscribe = HM.lookup unsub_id subs
          when (isJust m_unsubscribe) $ writeTVar subsVar $ HM.delete unsub_id subs
          return m_unsubscribe

        case m_unsubscribe of
          Nothing ->
            -- The protocol doesn't have a reply to unsub message
            -- Maybe log a warning?
            pass

          Just unsubscribe ->
            unsubscribe

      C_Method Method{method_method,method_params,method_id} -> do
        result <- callMethod appState method_method method_params
        sendMessage $ S_Result $ Result method_id result

debugConfig = Config connect callMethod subscribe
  where

    connect _ = putStrLn "connect"

    callMethod _ method args = do
      putStrLn $ "callMethod " ++ show method ++ " " ++ show args
      return (Right (object []))

    subscribe _ name args onEvent = do
      putStrLn $ "subscribe " ++ show name ++ " " ++ show args
      onEvent (E_Added $ Added "people" "id001" (object []))
      tid <- forkIO $ threadDelay 5000000 >> onEvent (E_Removed $ Removed "people" "id001")
      let unsubscribe = killThread tid >> print "unsub"
      return unsubscribe
