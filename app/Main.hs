{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Text.Blaze.Html.Renderer.Pretty as P
import Text.Blaze.Html.Renderer.Utf8 as U
import Text.Blaze.Html
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import Data.Yaml
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Index
import Media
import Movies
import Animes
import Series
import Util



mainLayout :: Html -> Html
mainLayout main_content = docTypeHtml $ do
    H.head $ do
        meta ! httpEquiv "content-type" ! content "text/html; charset=utf8"
        H.title "romes"
        link ! rel "stylesheet" ! href "style.css" ! media "all" ! type_ "text/css"
    body ! class_ "index-container" $ do

        nav $ do
            p ! class_  "logo" $ "romes" 
            ul $ do
                li $ a ! href "index.html" $ "index"
                li "blog"
                li "music"
                li "poetry"
                li "github"

        main_content



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
    indexMD <- TIO.readFile "data/index.md"
    BL.writeFile "docs/index.html" $ U.renderHtml $ mainLayout $ indexHtml mvs ams (mdtoNode indexMD)
