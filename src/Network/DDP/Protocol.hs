{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, LambdaCase #-}
module Network.DDP.Protocol where

import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Data.Aeson hiding (Result)

import Network.DDP.Protocol.TH

-- based on https://github.com/meteor/meteor/blob/7a4d6f66a0bb727ff20938479347415b1fa756a3/packages/ddp/DDP.md

newtype SubId = SubId { unSubId :: Text }
  deriving (Eq, Show, Ord, ToJSON, FromJSON)

newtype MethodCallId = MethodCallId { unMethodCallId :: Text }
  deriving (Eq, Show, Ord, ToJSON, FromJSON)

newtype Error = Error { unError :: Text }
  deriving (Eq, Show, Ord, ToJSON, FromJSON)

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
  { ubsub_id :: SubId
  } deriving (Eq, Show)

deriveFromJSON ''Unsub

data Method = Method
  { method_method :: Text
    -- ^ method name
  , method_params :: [Value]
  , method_id :: MethodCallId
  , randomSeed :: Maybe Value
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

data ServerMessage =
    S_Nosub Nosub
  | S_Added Added
  | S_Changed Changed
  | S_Removed Removed
  | S_Ready Ready
  -- TODO addedBefore, movedBefore
  | S_Result Result
  | S_Updated Updated

instance ToJSON ServerMessage where
  toJSON = \case
      S_Nosub   val -> msg "nosub"   val
      S_Added   val -> msg "added"   val
      S_Removed val -> msg "removed" val
      S_Changed val -> msg "changed" val
      S_Ready   val -> msg "ready"   val
      S_Result  val -> msg "result"  val
      S_Updated val -> msg "updated" val

    where
      msg :: ToJSON a => Text -> a -> Value
      msg name val =
        case toJSON val of
          Object obj ->
            object ("msg" .= name : HM.toList obj)
          _ -> error "Messages should be serialized to JSON objects"

data ClientMessage =
    C_Sub Sub
  | C_Unsub Unsub
  | C_Method Method

instance FromJSON ClientMessage where
  parseJSON (Object obj) = _
