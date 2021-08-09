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
    yearNew     :: Integer,
    artistNew   :: Text,
    date     :: Day
} deriving (Show, Eq, Generic)

data AlbumOld = AlbumOld {
    name    :: Text,
    artist    :: Text,
    cover     :: Integer,
    year   :: Text,
    month   :: Integer,
    day     :: Day
} deriving (Show, Eq, Generic)

instance ToJSON Album
instance FromJSON Album

instance ToJSON AlbumOld
instance FromJSON AlbumOld

instance Media Album where
    mediaAttrNames _ = ["year", "title", "artist"]
    mediaAttrs (Album title year artist date) = [
        toHtml year,
        toHtml title,
        toHtml artist
                                                ]
    getDate a = date a

convert :: 
