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
import Data.Time

import Index
import Media
import Movies
import Albums
import Animes
import Series
import Util



mainLayout :: Bool -> Html -> Html
mainLayout is_translation main_content = docTypeHtml $ do
    H.head $ do
        meta ! httpEquiv "content-type" ! content "text/html; charset=utf8"
        H.title "romes"
        link ! rel "stylesheet" ! href (relPathFix "style.css") ! media "all" ! type_ "text/css"
    body ! class_ "index-container" $ do

        nav $ do
            p ! class_  "logo" $ "romes" 
            ul $ do
                li $ a ! href "index.html" $ "index"
                li "blog"
                li "music"
                li "poetry"
                li "github"
                br
                li "-------"
                li $ "|" <> (a ! href (relPathFix "index.html") $ "en") <> "|" <> (a ! href (relPathFix "de/index.html") $ "de") <> "|"
                li "-------"
                li $ "|" <> (a ! href (relPathFix "ja/index.html") $ "ja") <> "|" <> (a ! href (relPathFix "ru/index.html") $ "ru") <> "|"
                li "-------"
        main_content

    where
        relPathFix = if is_translation then ("../" <>) else Prelude.id



main :: IO ()
main = do
    today <- utctDay <$> getCurrentTime

    -- Movies
    Right mvs <- decodeFileEither "data/movies.yaml"
    BL.writeFile "docs/movies.html" $ U.renderHtml $ mediaToHtml (mvs :: [Movie])

    -- Animes
    Right ams <- decodeFileEither "data/animes.yaml"
    BL.writeFile "docs/animes.html" $ U.renderHtml $ mediaToHtml (ams :: [Anime])

    -- Series
    Right srs <- decodeFileEither "data/series.yaml"
    BL.writeFile "docs/series.html" $ U.renderHtml $ mediaToHtml (srs :: [Serie])

    -- Albums
    Right abs <- decodeFileEither "data/albums.yaml"
    BL.writeFile "docs/albums.html" $ U.renderHtml $ mediaToHtml (abs :: [Album])

    -- Index
    indexMD <- TIO.readFile "data/index.md"
    BL.writeFile "docs/index.html" $ U.renderHtml $ mainLayout False $ indexHtml today mvs (mdtoNode indexMD)

    indexMD <- TIO.readFile "data/de/index.md"
    BL.writeFile "docs/de/index.html" $ U.renderHtml $ mainLayout True $ indexHtml today mvs (mdtoNode indexMD)
