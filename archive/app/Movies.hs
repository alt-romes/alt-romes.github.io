{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Movies where

import Text.Printf (printf)
import GHC.Generics
import Data.Text as T
import Data.Time
import Data.Yaml as Y
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Media

data Movie = Movie {
    title    :: Text,
    year     :: Integer,
    director :: Text,
    rating   :: Integer,
    date     :: Day
} deriving (Show, Eq, Generic)

instance ToJSON Movie
instance FromJSON Movie

instance Media Movie where
    mediaAttrNames _ = ["**", "movie", "year", "director"]
    mediaAttrs (Movie title year director rating date) = [
        H.span ! class_ "yellow" $ toHtml (printf "%02d" rating :: String),
        toHtml title,
        toHtml year,
        toHtml director
                                                         ] 
    getDate m = date m
