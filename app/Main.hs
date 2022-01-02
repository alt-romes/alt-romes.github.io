{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.List as L
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
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Data.Yaml
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import System.Console.Haskeline
import System.Directory

import Index
import Media
import Movies
import Albums
import Animes
import Series
import Util
import Page
import Post



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
            Right mvs <- decodeFileIO "data/movies.yaml"
            writeHtmlFileIO "docs/movies.html" $ makePage (mvs :: [Movie])

            -- Animes
            outputStrLn "Building animes..."
            Right ams <- decodeFileIO "data/animes.yaml"
            writeHtmlFileIO "docs/animes.html" $ makePage (ams :: [Anime])

            -- Series
            outputStrLn "Building series..."
            Right srs <- decodeFileIO "data/series.yaml"
            writeHtmlFileIO "docs/series.html" $ makePage (srs :: [Serie])

            -- Albums
            outputStrLn "Building albums..."
            Right abs <- decodeFileIO "data/albums.yaml"
            writeHtmlFileIO "docs/albums.html" $ makePage (abs :: [Album])

            -- Posts
            outputStrLn "Building posts..."
            postsPaths <- filter (isSuffixOf ".md") <$> liftIO (getDirectoryContents "data/posts")
            posts <- Posts <$> forM postsPaths (\postPath -> do
                outputStrLn $ "Building " <> postPath
                post <- Post postPath <$> liftIO (TIO.readFile $ "data/posts/" <> postPath)
                writeHtmlFileIO ("docs/posts/" <> dropSuffix ".md" postPath <> ".html") $ makePageWithNav post
                return post)

            -- Posts index
            outputStrLn "Building posts index..."
            writeHtmlFileIO "docs/posts.html" $ makePageWithNav posts
            -- Index
            outputStrLn "Building index..."
            indexMD <- liftIO $ TIO.readFile "data/index.md"
            writeHtmlFileIO "docs/index.html" $ makePageWithNav $ Index today mvs indexMD

            -- Style
            outputStrLn "Adding style..."
            liftIO (TIO.readFile "assets/style.css" >>= TIO.writeFile "docs/style.css")

            outputStrLn "Done."
            loop

          Just "add movie" -> addMovie
          Just "add anime" -> addAnime
          Just "add serie" -> addSerie
          Just "add album" -> addAlbum

          Just input -> do
            outputStrLn $ "Unknown command \"" ++ input ++ "\"."
            loop


    decodeFileIO filePath = liftIO $ decodeFileEither filePath
    writeHtmlFileIO :: String -> Html -> InputT IO ()
    writeHtmlFileIO filePath html = liftIO $ BL.writeFile filePath $ U.renderHtml html


    validateInputs :: [Maybe String] -> [Int] -> ([String] -> InputT IO ()) -> InputT IO ()
    validateInputs maybeInputs toIntegerIndexes successFun =
        if any isNothing maybeInputs
          then void (outputStrLn "Failed - input can't be empty!") >> loop
          else
            let inputs = catMaybes maybeInputs in
            if any isNothing (Prelude.map (TR.readMaybe . (inputs !!)) toIntegerIndexes :: [Maybe Integer])
            then void (outputStrLn $ "Failed - fields " <> show toIntegerIndexes <> " must be numeric.") >> loop
            else successFun inputs


    addMovie, addAnime, addSerie, addAlbum :: InputT IO ()
    addMovie = do
        today <- liftIO $ utctDay <$> getCurrentTime
        maybeInputs <- mapM (\f -> getUserInput ("Movie " <> f) "movie> ") ["title:", "year:", "director:", "rating (1-10):"]
        validateInputs maybeInputs [1, 3] $ \inputs -> do
              outputStrLn "Writing new movie to list..."
              let new_movie = Movie (T.pack $ Prelude.head inputs) (read $ inputs !! 1) (T.pack $ inputs !! 2) (read $ inputs !! 3) today
              Right mvs <- liftIO $ decodeFileEither "data/movies.yaml"
              liftIO $ encodeFile "data/movies.yaml" (new_movie:mvs)
              outputStrLn "Done."
              loop

    addAnime = do
        today <- liftIO $ utctDay <$> getCurrentTime
        maybeInputs <- mapM (\f -> getUserInput ("Anime " <> f) "anime> ") ["title:", "year:", "studio:", "rating (1-10):"]
        validateInputs maybeInputs [1, 3] $ \inputs -> do
              outputStrLn "Writing new anime to list..."
              let new_anime = Anime (T.pack $ Prelude.head inputs) (read $ inputs !! 1) (T.pack $ inputs !! 2) (read $ inputs !! 3) today
              Right ams <- liftIO $ decodeFileEither "data/animes.yaml"
              liftIO $ encodeFile "data/animes.yaml" (new_anime:ams)
              outputStrLn "Done."
              loop

    addSerie = do
        today <- liftIO $ utctDay <$> getCurrentTime
        maybeInputs <- mapM (\f -> getUserInput ("Serie " <> f) "serie> ") ["title:", "year:", "creator:", "rating (1-10):"]
        validateInputs maybeInputs [1, 3] $ \inputs -> do
              outputStrLn "Writing new serie to list..."
              let new_serie = Serie (T.pack $ Prelude.head inputs) (read $ inputs !! 1) (T.pack $ inputs !! 2) (read $ inputs !! 3) today
              Right srs <- liftIO $ decodeFileEither "data/series.yaml"
              liftIO $ encodeFile "data/series.yaml" (new_serie:srs)
              outputStrLn "Done."
              loop

    addAlbum = do
        today <- liftIO $ utctDay <$> getCurrentTime
        maybeInputs <- mapM (\f -> getUserInput ("Album " <> f) "album> ") ["title:", "year:", "artist:", "mark? (y/n)"]
        validateInputs maybeInputs [1] $ \inputs -> do
              outputStrLn "Writing new album to list..."
              let new_album = Album (T.pack $ Prelude.head inputs) (read $ inputs !! 1) (T.pack $ inputs !! 2) (let mark = T.toLower $ T.pack $ inputs !! 3 in mark == "yes" || mark == "y") today
              Right abs <- liftIO $ decodeFileEither "data/albums.yaml"
              liftIO $ encodeFile "data/albums.yaml" (new_album:abs)
              outputStrLn "Done."
              loop
