{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, LambdaCase #-}
module Network.DDP.Protocol where

import Control.Monad
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Aeson hiding (Result)

import Network.DDP.Protocol.TH

-- based on https://github.com/meteor/meteor/blob/7a4d6f66a0bb727ff20938479347415b1fa756a3/packages/ddp/DDP.md

newtype SubId = SubId { unSubId :: Text }
  deriving (Eq, Show, Ord, ToJSON, FromJSON, Hashable)

newtype MethodCallId = MethodCallId { unMethodCallId :: Text }
  deriving (Eq, Show, Ord, ToJSON, FromJSON, Hashable)

newtype SessionId = SessionId { unSessionId :: Text }
  deriving (Eq, Show, Ord, ToJSON, FromJSON)

newtype ProtocolVersion = ProtocolVersion { unProtocolVersion :: Text }
  deriving (Eq, Show, Ord, ToJSON, FromJSON)

data Error = Error
  { error_error :: Text
  , error_reason :: Maybe Text
  , error_message :: Maybe Text
  , error_errorType :: Maybe Text
  } deriving (Eq, Show, Ord)

deriveFromJSON ''Error
deriveToJSON ''Error

type CollectionName = Text
type Id = Text

data Nosub = Nosub
  { nosub_id :: SubId
  , nosub_error :: Error
  } deriving (Eq, Show)

deriveToJSON ''Nosub

data Added = Added
  { added_collection :: CollectionName
  , added_id :: Id -- ^ document id
  , added_fields :: Value
  } deriving (Eq, Show)

deriveToJSON ''Added

data Changed = Changed
  { changed_collection :: CollectionName
  , changed_id :: Id
  , changed_fields :: Value
  , changed_cleared :: [Text] -- ^ names of cleared fields
  } deriving (Eq, Show)

deriveToJSON ''Changed

data Removed = Removed
  { removed_collection :: CollectionName
  , removed_id :: Id
  } deriving (Eq, Show)

deriveToJSON ''Removed

data Ready = Ready
  { ready_subs :: [SubId]
  } deriving (Eq, Show)

deriveToJSON ''Ready

data Sub = Sub
  { sub_id :: SubId
  , sub_name :: Text
    -- ^ subscription name
  , sub_params :: [Value]
    -- ^ extra parameters to the subscription handler
  } deriving (Eq, Show)

deriveFromJSON ''Sub

data Unsub = Unsub
  { unsub_id :: SubId
  } deriving (Eq, Show)

deriveFromJSON ''Unsub

data Method = Method
  { method_method :: Text
    -- ^ method name
  , method_params :: [Value]
  , method_id :: MethodCallId
  , method_randomSeed :: Maybe Value
  } deriving (Eq, Show)

deriveFromJSON ''Method

data Result = Result
  { result_id :: MethodCallId
  , result_result :: Either Error Value
  } deriving (Eq, Show)

deriveToJSON ''Result

data Updated = Updated
  { updated_methods :: [MethodCallId]
  } deriving (Eq, Show)

deriveToJSON ''Updated

data Ping = Ping
  { ping_id :: Text
  } deriving (Eq, Show)

deriveFromJSON ''Ping
deriveToJSON ''Ping

data Pong = Pong
  { pong_id :: Text
  } deriving (Eq, Show)

deriveFromJSON ''Pong
deriveToJSON ''Pong

data Connected = Connected
  { connected_session :: SessionId
  } deriving (Eq, Show)

deriveToJSON ''Connected

data Failed = Failed
  { failed_version :: ProtocolVersion
  } deriving (Eq, Show)

deriveToJSON ''Failed

data ErrorMsg = ErrorMsg
  { errormsg_reason :: Text
  , errormsg_offendingMessage :: Maybe Value
  } deriving (Eq, Show)

deriveToJSON ''ErrorMsg

data ServerMessage =
  -- Handshake
    S_Connected Connected
  | S_Failed Failed
  -- Heartbeats
  | S_Ping Ping
  | S_Pong Pong
  -- Data management (TODO: addedBefore, movedBefore)
  | S_Nosub Nosub
  | S_Added Added
  | S_Changed Changed
  | S_Removed Removed
  | S_Ready Ready
  -- RPC
  | S_Result Result
  | S_Updated Updated
  -- Errors
  | S_Error ErrorMsg
  deriving (Eq, Show)

instance ToJSON ServerMessage where
  toJSON = \case
      S_Connected val -> msg "connected" val
      S_Failed    val -> msg "failed"    val
      S_Ping      val -> msg "ping"      val
      S_Pong      val -> msg "pong"      val
      S_Nosub     val -> msg "nosub"     val
      S_Added     val -> msg "added"     val
      S_Removed   val -> msg "removed"   val
      S_Changed   val -> msg "changed"   val
      S_Ready     val -> msg "ready"     val
      S_Result    val -> msg "result"    val
      S_Updated   val -> msg "updated"   val
      S_Error     val -> msg "error"     val

    where
      msg :: ToJSON a => Text -> a -> Value
      msg name val =
        case toJSON val of
          Object obj ->
            object ("msg" .= name : HM.toList obj)
          _ -> error "Messages should be serialized to JSON objects"

data Connect = Connect
  { connect_session :: Maybe SessionId
  , connect_version :: ProtocolVersion
  , connect_support :: [ProtocolVersion]
  } deriving (Eq, Show)

deriveFromJSON ''Connect

data ClientMessage =
  -- Handshake
    C_Connect Connect
  -- Heartbeats
  | C_Ping Ping
  | C_Pong Pong
  -- Data management
  | C_Sub Sub
  | C_Unsub Unsub
  -- RPC
  | C_Method Method
  deriving (Eq, Show)

instance FromJSON ClientMessage where
  parseJSON (Object obj) = do
    typ <- obj .: "msg"
    case typ :: Text of
      "connect" -> C_Connect <$> parseJSON (Object obj)
      "ping"    -> C_Ping    <$> parseJSON (Object obj)
      "pong"    -> C_Pong    <$> parseJSON (Object obj)
      "sub"     -> C_Sub     <$> parseJSON (Object obj)
      "unsub"   -> C_Unsub   <$> parseJSON (Object obj)
      "method"  -> C_Method  <$> parseJSON (Object obj)
      _         -> mzero

  parseJSON _ = mzero
