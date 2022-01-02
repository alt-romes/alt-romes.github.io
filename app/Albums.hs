{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Albums where

import Text.Printf (printf)
import GHC.Generics
import Data.Text as T
import Data.Time
import Data.Yaml as Y
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Media

data Album = Album {
    title    :: Text,
    year     :: Integer,
    artist   :: Text,
    mark     :: Bool,
    date     :: Day
} deriving (Show, Eq, Generic)

instance ToJSON Album
instance FromJSON Album

instance Media Album where
    mediaAttrNames _ = ["*", "year", "title", "artist"]
    mediaAttrs (Album title year artist mark date) = [
        H.span ! class_ "purple" $ toHtml (if mark then "*" else " " :: Text),
        H.span ! class_ "yellow" $ toHtml year,
        toHtml title,
        toHtml artist
                                                ]
    getDate a = date a
