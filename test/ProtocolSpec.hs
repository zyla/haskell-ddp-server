module ProtocolSpec where

import Data.Text (Text)
import Data.Aeson as Aeson
import Test.Hspec
import TestInstances
import Network.DDP.Protocol as Protocol

spec :: Spec
spec = do
  describe "instance ToJSON Result" $ do
    it "serializes success case correctly" $ do
      toJSON (Result "id0" (Right $ Aeson.object []))
        `shouldBe` Aeson.object
          [ "id"     .= Aeson.String "id0"
          , "result" .= Aeson.object [] ]

    it "serializes error case correctly" $ do
      toJSON (Result "id0" (Left $ Protocol.Error "wat" Nothing Nothing Nothing))
        `shouldBe` Aeson.object
          [ "id"     .= Aeson.String "id0"
          , "error"  .= Aeson.object
              [ "error"      .= Aeson.String "wat"
              , "error_type" .= Aeson.Null
              , "reason"     .= Aeson.Null
              , "message"    .= Aeson.Null
              ] ]
