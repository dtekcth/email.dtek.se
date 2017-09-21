{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Module containing

module Alias (
  -- * Types
    Alias
  , alias
  , domain
  , forwardAddress
  , cid
  -- * Functions
  , allAliases
  , aliasesByDomain
  , aliasesByCid
  ) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Int (Int64)
import           Database.MySQL.Simple
import           Database.MySQL.Simple.Param
import           Data.Monoid ((<>))
import           Data.Maybe (fromMaybe)
import           GHC.Generics
import           Data.Text.Lazy (Text, toStrict)
import           Data.Text.Encoding (encodeUtf8)

-- * Types

data Alias = Alias { alias_id       :: Int64
                   , alias          :: Text
                   , domain         :: Text
                   , forwardAddress :: Text
                   , cid            :: Maybe Text
                   } deriving (Show, Generic)

instance ToJSON Alias
instance FromJSON Alias

-- * Exposed database functions

-- | Get all aliases in database specified in @conn@
allAliases :: Connection -> IO [Alias]
allAliases conn = toAliases <$> query_ conn (prepQuery [])

-- | Get all aliases in database specified in @conn@ that are on domain @dom@
aliasesByDomain :: Param p => Connection -> p -> IO [Alias]
aliasesByDomain conn dom = toAliases <$> query conn (prepQuery ["domain"]) [dom]

-- | Get all aliases in database specified in @conn@ belonging to user @cid@
aliasesByCid :: Param p => Connection -> p -> IO [Alias]
aliasesByCid conn field = toAliases <$> query conn (prepQuery ["cid"]) [field]

-- | Insert an alias into the database @conn@
insertAlias :: Connection -> Alias -> IO Int64
insertAlias conn (Alias _ al dom fwAddr cid) = execute conn "INSERT INTO aliases (alias, domain, forward_address, cid) VALUES (?, ?, ?, ?) " [al, dom, fwAddr, fromMaybe "" cid]

deleteAlias :: Connection -> Alias -> IO Int64
deleteAlias conn al = execute conn "DELETE FROM aliases where alias_id = ?" [alias_id al]

-- * Helping functions

prepQuery :: [Query] -> Query
prepQuery [] = "SELECT alias_id, alias, domain, forward_address, cid FROM aliases ORDER BY alias"
prepQuery fields = "SELECT alias_id, alias, domain, forward_address, cid FROM aliases WHERE "
  <> expand fields <> " ORDERY BY alias"
  where expand :: [Query] -> Query
        expand []     = ""
        expand (f:[]) = f <> " = ? "
        expand (f:fs) = f <> " = ? and "

toAliases :: [(Int64, Text, Text, Text, Maybe Text)] -> [Alias]
toAliases = map (uncurry5 Alias)

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f (a, b, c, d, e) = f a b c d e
