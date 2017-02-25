-- | Helpers for deriving ToJSON and FromJSON instances.
module Network.DDP.Protocol.TH (
    deriveJSON
) where

import Data.Char (isUpper, toLower)
import Language.Haskell.TH.Syntax
import qualified Data.Aeson.TH as Aeson

-- Convert Haskell field name like prefix_fieldName to field_name.
fieldNameToJSON :: String -> String
fieldNameToJSON fieldName = camelToSnake $ dropPrefix fieldName
    where dropPrefix [] = fieldName
          dropPrefix ('_':xs) = xs
          dropPrefix (_:xs) = dropPrefix xs

          camelToSnake (x:xs)
            | isUpper x = '_' : Data.Char.toLower x : camelToSnake xs
            | otherwise = x : camelToSnake xs
          camelToSnake [] = []

-- Options to use with deriveJSON
jsonOptions :: Aeson.Options
jsonOptions = Aeson.defaultOptions
    { Aeson.fieldLabelModifier = fieldNameToJSON
    , Aeson.allNullaryToStringTag = True }

deriveJSON :: Name -> Q [Dec]
deriveJSON = Aeson.deriveJSON jsonOptions
