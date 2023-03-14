{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Series where

import Text.Printf (printf)
import GHC.Generics
import Data.Text as T
import Data.Time
import Data.Yaml as Y
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Media

data Serie = Serie {
    title    :: Text,
    year     :: Integer,
    creator  :: Text,
    rating   :: Integer,
    date     :: Day
} deriving (Show, Eq, Generic)

instance ToJSON Serie
instance FromJSON Serie

instance Media Serie where
    mediaAttrNames _ = ["**", "series", "year", "creator"]
    mediaAttrs (Serie title year creator rating date) = [
        H.span ! class_ "yellow" $ toHtml (printf "%02d" rating :: String),
        toHtml title,
        toHtml year,
        toHtml creator
                                                       ]
    getDate s = date s

