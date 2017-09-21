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

import Alias

main :: IO ()
main = do
    conn <- connect $ defaultConnectInfo { connectDatabase = "email" }
    scotty 3000 $ routes conn

routes :: Connection -> ScottyM ()
routes conn = undefined
