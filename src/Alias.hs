{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Module containing

module Alias (
  -- * Types
    Alias
  , toAlias
  , aliasId
  , alias
  , domain
  , forwardAddress
  , cid
  ) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Int (Int64)
import           GHC.Generics
import           Data.Text.Lazy (Text)

-- * Types

data Alias = Alias { aliasId        :: Int64
                   , alias          :: Text
                   , domain         :: Text
                   , forwardAddress :: Text
                   , cid            :: Maybe Text
                   } deriving (Show, Generic)

toAlias :: Int64 -> Text -> Text -> Text -> Maybe Text -> Alias
toAlias = Alias

instance ToJSON Alias
instance FromJSON Alias
