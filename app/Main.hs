{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL
import           Database.MySQL.Simple
import           GHC.Generics
import           Web.Scotty

data Alias = Alias { alias  :: TL.Text
                   , domain :: TL.Text
                   , forwardAddress :: TL.Text
                   , cid :: Maybe TL.Text
                   } deriving (Show, Generic)

instance ToJSON Alias
instance FromJSON Alias

toAlias :: TL.Text -> TL.Text -> TL.Text -> Maybe TL.Text -> Alias
toAlias = Alias

main :: IO ()
main = do
    conn <- connect $ defaultConnectInfo { connectDatabase = "email" }
    scotty 3000 $ routes conn

routes :: Connection -> ScottyM ()
routes conn = do
  get "/" $ do
    rs <- liftIO $ query_ conn queryAll
    json . toAliases $ rs

  get "/:cid" $ do
    cid <- param "cid" :: ActionM TL.Text
    rs <- liftIO $ query conn (queryWithField "cid") [cid]
    json . toAliases $ rs

  get "/domain/:domain" $ do
    domain <- param "domain" :: ActionM TL.Text
    rs <- liftIO $ query conn (queryWithField "domain") [domain]
    json . toAliases $ rs

  where toAliases :: [(TL.Text, TL.Text, TL.Text, Maybe TL.Text)] -> [Alias]
        toAliases = map (uncurry4 toAlias)

        uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
        uncurry4 f (a, b, c, d) = f a b c d

        qStart = "SELECT alias, domain, forward_address, cid FROM aliases "
        qEnd = "ORDER BY alias"

        queryWithField :: Query -> Query
        queryWithField field = qStart <> " WHERE " <> field <> " = ? " <> qEnd

        queryAll :: Query
        queryAll = qStart <> qEnd
