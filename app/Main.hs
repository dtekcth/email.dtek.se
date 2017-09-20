{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid
import Data.Maybe
import qualified Data.Text.Lazy as TL
import Database.MySQL.Simple
import Web.Scotty

import Lib

main :: IO ()
main = do
    conn <- connect $ defaultConnectInfo { connectDatabase = "email" }
    scotty 3000 $ do
        get "/admin" $ do
            rs <- liftIO $ query_ conn "SELECT alias, domain, forward_address, cid FROM aliases ORDER BY alias"
            html $
                "<h1>Admin: E-mail alias management"
                <> "<table>"
                <> tråkig rs
                <> "</table>"

tråkig :: [(TL.Text, TL.Text, TL.Text, Maybe TL.Text)] -> TL.Text
tråkig = mconcat . map (\(a,d,f,c) ->
    "<tr>"
        <> "<td>" <> a <> "</td>"
        <> "<td>" <> d <> "</td>"
        <> "<td>" <> f <> "</td>"
        <> "<td>" <> fromMaybe "" c <> "</td>"
      <> "</tr>"
     )
