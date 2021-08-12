{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Either
import qualified Text.Read as TR
import Data.Maybe
import Control.Monad.Trans
import Control.Monad
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
import System.Console.Haskeline

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
main = runInputT defaultSettings loop
    where
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "romes> "
        case minput of
          Nothing -> return ()
          Just ":q" -> return ()
          Just "" -> loop
          Just "help" -> do
              outputStrLn "help:\n    add\n    make\n    help"
              loop
          Just "add" -> do
              outputStrLn "add:\n    add movie\n    add anime\n    add serie\n    add album"
              loop
          Just "make" -> do
            today <- liftIO $ utctDay <$> getCurrentTime

            -- Movies
            outputStrLn "Building movies..."
            Right mvs <- liftIO $ decodeFileEither "data/movies.yaml"
            liftIO $ BL.writeFile "docs/movies.html" $ U.renderHtml $ mediaToHtml (mvs :: [Movie])

            -- Animes
            outputStrLn "Building animes..."
            Right ams <- liftIO $ decodeFileEither "data/animes.yaml"
            liftIO $ BL.writeFile "docs/animes.html" $ U.renderHtml $ mediaToHtml (ams :: [Anime])

            -- Series
            outputStrLn "Building series..."
            Right srs <- liftIO $ decodeFileEither "data/series.yaml"
            liftIO $ BL.writeFile "docs/series.html" $ U.renderHtml $ mediaToHtml (srs :: [Serie])

            -- Albums
            outputStrLn "Building albums..."
            Right abs <- liftIO $ decodeFileEither "data/albums.yaml"
            liftIO $ BL.writeFile "docs/albums.html" $ U.renderHtml $ mediaToHtml (abs :: [Album])

            -- Index
            outputStrLn "Building index..."
            indexMD <- liftIO $ TIO.readFile "data/index.md"
            liftIO $ BL.writeFile "docs/index.html" $ U.renderHtml $ mainLayout False $ indexHtml today mvs (mdtoNode indexMD)

            indexMD <- liftIO $ TIO.readFile "data/de/index.md"
            liftIO $ BL.writeFile "docs/de/index.html" $ U.renderHtml $ mainLayout True $ indexHtml today mvs (mdtoNode indexMD)

            outputStrLn "Done."
            loop

          Just "add movie" -> do
            today <- liftIO $ utctDay <$> getCurrentTime
            maybeInputs <- mapM (\f -> getUserInput ("Movie " <> f) "movie> ") ["title:", "year:", "director:", "rating (1-10):"]
            if any isNothing maybeInputs
              then void (outputStrLn "Failed - input can't be empty!") >> loop
              else
                let inputs = catMaybes maybeInputs in
                if any isNothing [TR.readMaybe $ inputs !! 1 :: Maybe Integer, TR.readMaybe $ inputs !! 3 :: Maybe Integer]
                then void (outputStrLn "Failed - year and rating should be numeric.") >> loop
                else do
                  outputStrLn "Writing new movie to list..."
                  let new_movie = Movie (T.pack $ Prelude.head inputs) (read $ inputs !! 1) (T.pack $ inputs !! 2) (read $ inputs !! 3) today
                  Right mvs <- liftIO $ decodeFileEither "data/movies.yaml"
                  liftIO $ encodeFile "data/movies.yaml" (new_movie:mvs)
                  outputStrLn "Done."
                  loop

          Just "add anime" -> do
            today <- liftIO $ utctDay <$> getCurrentTime
            maybeInputs <- mapM (\f -> getUserInput ("Anime " <> f) "anime> ") ["title:", "year:", "studio:", "rating (1-10):"]
            if any isNothing maybeInputs
              then void (outputStrLn "Failed - input can't be empty!") >> loop
              else
                let inputs = catMaybes maybeInputs in
                if any isNothing [TR.readMaybe $ inputs !! 1 :: Maybe Integer, TR.readMaybe $ inputs !! 3 :: Maybe Integer]
                then void (outputStrLn "Failed - year and rating should be numeric.") >> loop
                else do
                  outputStrLn "Writing new anime to list..."
                  let new_anime = Anime (T.pack $ Prelude.head inputs) (read $ inputs !! 1) (T.pack $ inputs !! 2) (read $ inputs !! 3) today
                  Right ams <- liftIO $ decodeFileEither "data/animes.yaml"
                  liftIO $ encodeFile "data/animes.yaml" (new_anime:ams)
                  outputStrLn "Done."
                  loop

          Just input -> do
            outputStrLn $ "Unknown command \"" ++ input ++ "\"."
            loop










