--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString
import Data.ByteString.Lazy
import Data.Yaml

import Hakyll

import Media


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $
        compile templateBodyCompiler

    -- match "data/*" $ do
    --     route $ setExtension "html"
    --     compile $ dataCompiler
    --         >>= loadAndApplyTemplate "templates/data.html" defaultContext
    --         >>= relativizeUrls 

    match "archive/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "archive/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do

            posts <- recentFirst =<< loadAll "archive/*"
            let indexCtx = listField "posts" postCtx (return posts) <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

dataCompiler :: (FromJSON a, Media a) => Compiler (Item a)
dataCompiler = do
    content <- getResourceLBS
    return (fromRight . decodeEither' . toStrict <$> content)
    where
    fromRight (Right x) = x
    fromRight _ = error "fromRight"
