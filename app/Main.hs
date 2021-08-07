{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Text.Blaze.Html.Renderer.Pretty as P
import Text.Blaze.Html.Renderer.Utf8 as U
import Data.Yaml

import Index
import Media
import Movies
import Animes
import Series

main :: IO ()
main = do
    -- Movies
    Right mvs <- decodeFileEither "data/movies.yaml"
    BL.writeFile "docs/movies.html" $ U.renderHtml $ mediaToHtml (mvs :: [Movie])

    -- Animes
    Right ams <- decodeFileEither "data/animes.yaml"
    BL.writeFile "docs/animes.html" $ U.renderHtml $ mediaToHtml (ams :: [Anime])

    -- Series
    Right srs <- decodeFileEither "data/series.yaml"
    BL.writeFile "docs/series.html" $ U.renderHtml $ mediaToHtml (srs :: [Serie])

    -- Index
    BL.writeFile "docs/index.html" $ U.renderHtml indexHtml
