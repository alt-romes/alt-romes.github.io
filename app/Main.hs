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
import Projects



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

            outputStrLn "Building movies..."
            mvs <- decodeFileIO "data/movies.yaml"
            writeHtmlPageIO "docs/movies.html" (mvs :: [Movie])

            outputStrLn "Building animes..."
            ams <- decodeFileIO "data/animes.yaml"
            writeHtmlPageIO "docs/animes.html" (ams :: [Anime])

            outputStrLn "Building series..."
            srs <- decodeFileIO "data/series.yaml"
            writeHtmlPageIO "docs/series.html" (srs :: [Serie])

            outputStrLn "Building albums..."
            abs <- decodeFileIO "data/albums.yaml"
            writeHtmlPageIO "docs/albums.html" (abs :: [Album])

            outputStrLn "Building projects..."
            projs <- Projects <$> decodeFileIO "data/projects.yaml"
            writeHtmlPageIO "docs/projects.html" projs

            outputStrLn "Building posts..."
            postsPaths <- filter (isSuffixOf ".md") <$> liftIO (listDirectory "data/posts")
            posts <- PostsIndex <$> forM postsPaths (\postPath -> do
                outputStrLn $ "Building " <> postPath
                post <- Post postPath <$> liftIO (TIO.readFile $ "data/posts/" <> postPath)
                writeHtmlPageIO ("docs/posts/" <> dropSuffix ".md" postPath <> ".html") post
                return post)

            outputStrLn "Building posts index..."
            writeHtmlPageIO "docs/posts.html" posts

            outputStrLn "Building index..."
            indexMD <- liftIO $ TIO.readFile "data/index.md"
            writeHtmlPageIO "docs/index.html" $ Index today mvs indexMD

            outputStrLn "Adding assets..."
            assets <- liftIO (listDirectory "assets")
            forM_ assets $ \asset -> do
                outputStrLn ("Adding " <> asset <> "...")
                liftIO (BL.readFile ("assets/" <> asset) >>= BL.writeFile ("docs/" <> asset))

            outputStrLn "Done."
            loop

          Just "add movie" -> addMovie
          Just "add anime" -> addAnime
          Just "add serie" -> addSerie
          Just "add album" -> addAlbum

          Just input -> do
            outputStrLn $ "Unknown command \"" ++ input ++ "\"."
            loop


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
              mvs <- decodeFileIO "data/movies.yaml"
              liftIO $ encodeFile "data/movies.yaml" (new_movie:mvs)
              outputStrLn "Done."
              loop

    addAnime = do
        today <- liftIO $ utctDay <$> getCurrentTime
        maybeInputs <- mapM (\f -> getUserInput ("Anime " <> f) "anime> ") ["title:", "year:", "studio:", "rating (1-10):"]
        validateInputs maybeInputs [1, 3] $ \inputs -> do
              outputStrLn "Writing new anime to list..."
              let new_anime = Anime (T.pack $ Prelude.head inputs) (read $ inputs !! 1) (T.pack $ inputs !! 2) (read $ inputs !! 3) today
              ams <- decodeFileIO "data/animes.yaml"
              liftIO $ encodeFile "data/animes.yaml" (new_anime:ams)
              outputStrLn "Done."
              loop

    addSerie = do
        today <- liftIO $ utctDay <$> getCurrentTime
        maybeInputs <- mapM (\f -> getUserInput ("Serie " <> f) "serie> ") ["title:", "year:", "creator:", "rating (1-10):"]
        validateInputs maybeInputs [1, 3] $ \inputs -> do
              outputStrLn "Writing new serie to list..."
              let new_serie = Serie (T.pack $ Prelude.head inputs) (read $ inputs !! 1) (T.pack $ inputs !! 2) (read $ inputs !! 3) today
              srs <- decodeFileIO "data/series.yaml"
              liftIO $ encodeFile "data/series.yaml" (new_serie:srs)
              outputStrLn "Done."
              loop

    addAlbum = do
        today <- liftIO $ utctDay <$> getCurrentTime
        maybeInputs <- mapM (\f -> getUserInput ("Album " <> f) "album> ") ["title:", "year:", "artist:", "mark? (y/n)"]
        validateInputs maybeInputs [1] $ \inputs -> do
              outputStrLn "Writing new album to list..."
              let new_album = Album (T.pack $ Prelude.head inputs) (read $ inputs !! 1) (T.pack $ inputs !! 2) (let mark = T.toLower $ T.pack $ inputs !! 3 in mark == "yes" || mark == "y") today
              abs <- decodeFileIO "data/albums.yaml"
              liftIO $ encodeFile "data/albums.yaml" (new_album:abs)
              outputStrLn "Done."
              loop

    decodeFileIO filePath = fromRight (error ("Error decoding file " <> filePath)) <$> liftIO (decodeFileEither filePath)
    writeHtmlPageIO path page = liftIO $ writeHtmlPage path page

