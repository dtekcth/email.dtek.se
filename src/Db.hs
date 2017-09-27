{-# LANGUAGE OverloadedStrings #-}
-- | All database related stuff

module Db
  ( allAliases
  , aliasesByDomain
  , aliasesByCid
  , insertAlias
  , deleteAlias
  )
where

import           Data.Int (Int64)
import           Database.MySQL.Simple
import           Database.MySQL.Simple.Param
import           Data.Monoid ((<>))
import           Data.Maybe (fromMaybe)
import           Data.Text.Lazy (Text)

import Alias


-- | Get all aliases in database specified in @conn@
allAliases :: Connection -> IO [Alias]
allAliases conn = toAliases <$> query_ conn allAliasSQL

-- | Get all aliases in database specified in @conn@ that are on domain @dom@
aliasesByDomain :: Param p => Connection -> p -> IO [Alias]
aliasesByDomain conn dom = toAliases <$> query conn (filteredAliasSQL ["domain"]) [dom]

-- | Get all aliases in database specified in @conn@ belonging to user @cid@
aliasesByCid :: Param p => Connection -> p -> IO [Alias]
aliasesByCid conn field = toAliases <$> query conn (filteredAliasSQL ["cid"]) [field]

-- | Insert an alias into the database @conn@
insertAlias :: Connection -> Alias -> IO Int64
insertAlias conn a = execute conn insertAliasSQL
                     [alias a, domain a, forwardAddress a, fromMaybe "" $ cid a]

-- | Delete an alias from the database @conn@ with the id specified in @al@
deleteAlias :: Connection -> Alias -> IO Int64
deleteAlias conn al = execute conn deleteAliasSQL [aliasId al]

-- * Helping functions
toAliases :: [(Int64, Text, Text, Text, Maybe Text)] -> [Alias]
toAliases = map (uncurry5 toAlias)
  where uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
        uncurry5 f (a, b, c, d, e) = f a b c d e

-- * Raw SQL query templates
insertAliasSQL :: Query
insertAliasSQL = "INSERT INTO aliases (alias, domain, forward_address, cid) \
                   \ VALUES (?, ?, ?, ?)"

deleteAliasSQL :: Query
deleteAliasSQL = "DELETE FROM aliases where alias_id = ?"

allAliasSQL :: Query
allAliasSQL = "SELECT alias_id, alias, domain, forward_address, cid FROM aliases ORDER BY alias"

filteredAliasSQL :: [Query] -> Query
filteredAliasSQL [] = allAliasSQL
filteredAliasSQL fields = "SELECT alias_id, alias, domain, forward_address, cid FROM aliases WHERE "
                          <> expand fields <> " ORDERY BY alias"
  where expand :: [Query] -> Query
        expand []     = ""
        expand (f:[]) = f <> " = ? "
        expand (f:fs) = f <> " = ? and "
