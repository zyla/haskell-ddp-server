import Control.Concurrent
import Network.WebSockets as WS
import Data.Aeson
import Network.DDP.Protocol
import Network.DDP.Server
import Network.DDP.Server.WebSocket

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

main = do
  server <- newServer debugConfig
  WS.runServer "0.0.0.0" 3001 (toWebSocketServer server)
