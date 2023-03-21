--------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Map (Map, singleton)
import Data.ByteString
import Data.ByteString.Lazy
import Data.Yaml
import Data.Text (Text)
import qualified Data.Text as T

import Text.Pandoc.Highlighting (styleToCss)
import Text.Pandoc.Options      (ReaderOptions (..), WriterOptions (..))
import Skylighting (Style(..), ToColor(..), TokenType(..), TokenStyle(..), defStyle)

import Hakyll
import Data.Functor.Identity
import qualified Text.Pandoc.Templates as Pandoc

-- import Media


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    -- Copy images
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Compress css stylesheets
    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    -- Make clay stylesheets
    match "css/*.hs" $ do
        route   $ setExtension "css"
        compile $ getResourceString >>= withItemBody (unixFilter "cabal" ["exec", "runghc"])

    -- Create syntax stylesheet
    create ["css/syntax.css"] $ do
        route   idRoute
        compile $
            makeItem (styleToCss pandocCodeStyle)

    -- Compile templates
    match "templates/*" $
        compile templateBodyCompiler

    -- match "data/*" $ do
    --     route $ setExtension "html"
    --     compile $ dataCompiler
    --         >>= loadAndApplyTemplate "templates/data.html" defaultContext
    --         >>= relativizeUrls 

    -- match "assignments/*" $ do
    --     route $ setExtension "html"
    --     compile $ pandocCompilerS
    --         >>= loadAndApplyTemplate "templates/post.html"    postCtx
    --         >>= loadAndApplyTemplate "templates/default.html" postCtx
    --         >>= relativizeUrls

    -- Archive (unlisted)
    match "archive/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompilerS
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    -- create ["archive.html"] $ do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "archive/*"
    --         let archiveCtx =
    --                 listField "posts" postCtx (return posts) <>
    --                 constField "title" "Archives"            <>
    --                 defaultContext

    --         makeItem ""
    --             >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --             >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --             >>= relativizeUrls

    -- match "papers.md" $ do
    --     route $ setExtension "html"
    --     compile $ pandocCompilerS
    --         >>= loadAndApplyTemplate "templates/default.html" defaultContext
    --         >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerS
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Posts"               <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do

            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx = listField "posts" postCtx (return posts) <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx -- TODO: Index template, use posts from context?
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
            { writerHighlightStyle  = Just pandocCodeStyle
            , writerTableOfContents = True
            , writerNumberSections  = True
            , writerTOCDepth        = 2
            , writerTemplate        = Just tocTemplate
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

        (=:) :: TokenType -> TokenStyle -> Map TokenType TokenStyle
        (=:) = Data.Map.singleton

-- dataCompiler :: (FromJSON a, Media a) => Compiler (Item a)
-- dataCompiler = do
--     content <- getResourceLBS
--     return (fromRight . decodeEither' . toStrict <$> content)
--     where
--     fromRight (Right x) = x
--     fromRight _ = error "fromRight"

config :: Configuration
config = defaultConfiguration
           { destinationDirectory = "docs"
           }

tocTemplate :: Pandoc.Template Text
tocTemplate = either error id . runIdentity . Pandoc.compileTemplate "" $ T.unlines
  [ "<div class=\"toc\"><div class=\"header\">Contents</div>"
  ,     "$toc$"
  , "</div>"
  , "$body$"
  ]
