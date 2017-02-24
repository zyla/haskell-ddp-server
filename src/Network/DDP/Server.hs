module Network.DDP.Server where

import Data.IORef
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import Network.DDP.Protocol
import Control.Concurrent.STM

type Unsubscribe = IO ()

data Config = Config
  { callMethod :: Connection -> Text -> [Value] -> IO (Either Error Value)
  , subscribe :: Connection -> Text -> [Value] -> IO Unsubscribe
  }

data Server = Server { server_config :: Config }

newServer :: Config -> IO Server
newServer = return . Server

data Connection = Connection
  { connection_config :: Config
  , connection_subs :: TVar ConnectionState
  , connection_sendMessage :: ServerMessage -> IO ()
  }

data ConnectionState =
    New
  | Connected { subs :: TVar (HM.HashMap SubId Unsubscribe) }

newConnection :: Server -> (ServerMessage -> IO ()) ->  IO Connection
newConnection server sendMessage  = do
  state <- newTVarIO New
  return Connection
    { connection_config = server_config server
    , connection_state = state
    , connection_sendMessage = sendMessage
    }

processMessage :: Connection -> ClientMessage -> IO ()
processMessage conn@(Connection Config{callMethod,subscribe} stateVar _) msg = do
  state <- atomically $ readTVar stateVar
  case state of
    New -> case msg of
      C_Connect{} ->
        -- TODO connect
        return ()

      _ ->
        -- TODO: return "not connected" error
        return ()

    Connected subsVar -> case msg of

      C_Ping (Ping id) ->
        sendMessage conn (S_Pong (Pong id))

      C_Pong {} ->
        -- Timeouts should be handled during receive,
        -- so 'pong' serves only as a dummy packet
        return ()

      C_Sub Sub{sub_id,sub_name,sub_params} -> do
        unsub <- subscribe conn sub_name sub_params
        -- FIXME doesn't handle duplicate subId case
        atomically $ modifyTVar subsVar $ HM.insert sub_id unsub

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
            return ()

          Just unsubscribe ->
            unsubscribe

      C_Method Method{method_method,method_params,method_id} -> do
        result <- callMethod conn method_method method_params
        sendMessage conn $ S_Result $ Result method_id result

sendMessage :: Connection -> ServerMessage -> IO ()
sendMessage = connection_sendMessage

debugConfig = Config callMethod subscribe
  where
    callMethod _ _ _ = print "callMethod" >> return (Right (object []))
    subscribe _ _ _ = print "subscribe" >> return (print "unsub")

-- subscribe :: (Event -> IO ()) -> IO Unsubscribe
