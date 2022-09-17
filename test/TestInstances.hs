module TestInstances where

import Data.String
import Network.DDP.Protocol

instance IsString MethodCallId where
  fromString = MethodCallId . fromString
