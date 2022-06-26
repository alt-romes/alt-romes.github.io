--------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Map (Map, singleton)
import Data.ByteString
import Data.ByteString.Lazy
import Data.Yaml

import Text.Pandoc.Highlighting (styleToCss)
import Text.Pandoc.Options      (ReaderOptions (..), WriterOptions (..))
import Skylighting (Style(..), ToColor(..), TokenType(..), TokenStyle(..), defStyle)

import Hakyll

import Media


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    create ["css/syntax.css"] $ do
        route   idRoute
        compile $
            makeItem (styleToCss pandocCodeStyle)

    match "templates/*" $
        compile templateBodyCompiler

    -- match "data/*" $ do
    --     route $ setExtension "html"
    --     compile $ dataCompiler
    --         >>= loadAndApplyTemplate "templates/data.html" defaultContext
    --         >>= relativizeUrls 

    match "assignments/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerS
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "archive/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerS
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

    match "papers.md" $ do
        route $ setExtension "html"
        compile $ pandocCompilerS
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
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

-- | Styled pandoc compiler
pandocCompilerS :: Compiler (Item String)
pandocCompilerS =
    pandocCompilerWith
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
            { writerHighlightStyle = Just pandocCodeStyle
            }

-- | Custom style inspired by mexican-light
pandocCodeStyle :: Style
pandocCodeStyle = Style {..}
    where
        tokenStyles =
            CommentTok =: color "#b8b8b8" <>
            KeywordTok =: defStyle { tokenColor = toColor @String "#96609e", tokenBold = True } <>
            DataTypeTok =: color "#96609e" <>
            DecValTok =: color "#dc9656" <>
            StringTok =: color "#538947" <>
            CharTok =: color "#538947"
        defaultColor = toColor @String "#383838"
        backgroundColor = toColor @String "#f8f8f8"
        lineNumberColor = Nothing
        lineNumberBackgroundColor = Nothing
        color c = defStyle { tokenColor = toColor @String c }

dataCompiler :: (FromJSON a, Media a) => Compiler (Item a)
dataCompiler = do
    content <- getResourceLBS
    return (fromRight . decodeEither' . toStrict <$> content)
    where
    fromRight (Right x) = x
    fromRight _ = error "fromRight"

config :: Configuration
config = defaultConfiguration
           { destinationDirectory = "docs"
           }

(=:) :: TokenType -> TokenStyle -> Map TokenType TokenStyle
(=:) = Data.Map.singleton
