{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Animes where

import Text.Printf (printf)
import GHC.Generics
import Data.Text as T
import Data.Time
import Data.Yaml as Y
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Media

data Anime = Anime {
    title    :: Text,
    year     :: Integer,
    studio   :: Text,
    rating   :: Integer,
    date     :: Day
} deriving (Show, Eq, Generic)

instance ToJSON Anime
instance FromJSON Anime

instance Media Anime where
    mediaAttrNames _ = ["**", "anime", "year", "studio"]
    mediaAttrs (Anime title year studio rating date) = [
        H.span ! class_ "yellow" $ toHtml (printf "%02d" rating :: String),
        toHtml title,
        toHtml year,
        toHtml studio
                                                       ]


