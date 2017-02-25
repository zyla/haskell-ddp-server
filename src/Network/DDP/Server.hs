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

processMessage :: Connection s -> ClientMessage -> IO ()
processMessage (Connection Config{callMethod,subscribe,connect} stateVar sendMessage) msg = do
  -- FIXME: RACE CONDITION: Two threads may C_Connect at the same time.
  state <- atomically $ readTVar stateVar

  let respondError message =
        -- TODO: include offending message
        sendMessage (S_Error (ErrorMsg message Nothing))

  case state of
    New -> case msg of
      C_Connect{} -> do
        -- TODO check versions
        -- TODO handle sessions
        subsVar <- newTVarIO HM.empty
        appState <- connect ConnectionInfo
        sendMessage $ S_Connected $ Protocol.Connected $ SessionId ""
        atomically $ writeTVar stateVar $ Connected subsVar appState

      _ -> respondError "Not yet connected"

    Connected subsVar appState -> case msg of

      C_Connect{} ->
        respondError "Already connected"

      C_Ping (Ping id) ->
        sendMessage (S_Pong (Pong id))

      C_Pong {} ->
        -- Timeouts should be handled during receive,
        -- so 'pong' serves only as a dummy packet
        return ()

      C_Sub Sub{sub_id,sub_name,sub_params} -> do
        let onEvent event = sendMessage (fromDataEvent event)
            fromDataEvent = \case
              E_Added   val -> S_Added val
              E_Changed val -> S_Changed val
              E_Removed val -> S_Removed val

        -- See Note [Duplicate subscription IDs]

        -- TODO handle exceptions
        unsubscribe <- subscribe appState sub_name sub_params onEvent

        duplicate <- atomically $ do
          subs <- readTVar subsVar
          if HM.member sub_id subs
            then
              return True
            else do
              writeTVar subsVar $ HM.insert sub_id unsubscribe subs
              return False

        if duplicate
          then do
            -- cancel the subscription
            unsubscribe -- TODO handle exceptions during unsubscribe

            let error = Protocol.Error "duplicate_id" Nothing Nothing Nothing
            sendMessage $ S_Nosub $ Nosub sub_id error
          else
            sendMessage $ S_Ready $ Ready [sub_id]

      C_Unsub Unsub{unsub_id} -> do

        m_unsubscribe <- atomically $ do
          subs <- readTVar subsVar
          let m_unsubscribe = HM.lookup unsub_id subs
          when (isJust m_unsubscribe) $ writeTVar subsVar $ HM.delete unsub_id subs
          return m_unsubscribe

        case m_unsubscribe of
          Nothing ->
            respondError "Subscription ID not found"

          Just unsubscribe ->
            unsubscribe

      C_Method Method{method_method,method_params,method_id} -> do
        result <- callMethod appState method_method method_params
        sendMessage $ S_Result $ Result method_id result

{-

Note [Duplicate subscription IDs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With current API, there isn't a neat way to detect duplicate subscription IDs
in a thread-safe manner without subscribing. Why?
A solution would look like this:
@
 duplicate <- atomically $ do
   subs <- readTVar subsVar
   if HM.member sub_id subs
     then
       return True
     else do
       unsubscribe <- subscribe appState sub_name sub_params onEvent
       write subsVar $ HM.insert sub_id unsub subs
       return False

@

But 'subscribe' can't be embedded in STM.

Another solution is to just say that 'processMessage' is not thread safe.

-}
