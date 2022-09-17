module Main where

import Test.Hspec
import qualified ProtocolSpec

main = hspec $ do
  ProtocolSpec.spec
