module Network.DDP.Server.WebSocket where

import Data.Function (fix)
import qualified Network.WebSockets as WS
import Data.Aeson as Aeson
import Network.DDP.Protocol
import Network.DDP.Server as DDP

toWebSocketServer :: Server s -> WS.ServerApp
toWebSocketServer server = \pendingConnection -> do
  wsConnection <- WS.acceptRequest pendingConnection
  putStrLn $ "Got connection"

  let sendMessage :: ServerMessage -> IO ()
      sendMessage msg = do
        putStrLn $ "Sending: " ++ show msg
        WS.sendTextData wsConnection (Aeson.encode msg)

  connection <- DDP.newConnection server sendMessage

  fix $ \loop -> do
    -- TODO timeouts
    -- TODO errors
    messageBS <- WS.receiveData wsConnection
    putStrLn $ "Received: " ++ show messageBS

    case Aeson.decode @ClientMessage messageBS of

      Just message -> do
        print message
        DDP.processMessage connection message

      Nothing ->
        sendMessage (S_Error (ErrorMsg "Malformed message" Nothing))

    loop
