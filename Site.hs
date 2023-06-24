--------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Data.String
import Data.List (intersperse)
import Data.Map (Map, singleton)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set

import Text.Blaze.Html5.Attributes (href, class_, style)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 (toHtml, (!), toValue)
import qualified Text.Blaze.Html5 as H

import Text.Pandoc.Highlighting (styleToCss)
import Text.Pandoc.Options      (ReaderOptions (..), WriterOptions (..), Extension(..), enableExtension)
import Skylighting (Style(..), ToColor(..), TokenType(..), TokenStyle(..), defStyle)

import Hakyll
import Hakyll.Images (loadImage, compressJpgCompiler)
import Data.Functor.Identity
import qualified Text.Pandoc.Templates as Pandoc
import Data.Maybe (mapMaybe, isJust, catMaybes)

import System.FilePath.Posix

-- import Media


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do

    -- Copy images
    match "images/**.jpeg" $ do
        route   idRoute
        compile $ loadImage >>= compressJpgCompiler 100

    match "images/**" $ do
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

    -- Copy favicons to website root
    match "favicon/*" $ do
        route $ customRoute (takeFileName . toFilePath)
        compile copyFileCompiler

    -- Compile templates
    match "templates/*" $
        compile templateBodyCompiler

    tags <- buildTags "posts/**" (fromCapture "tags/*.html")

    let postCtx = postCtx' tags

    -- match "data/*" $ do
    --     route $ setExtension "html"
    --     compile $ dataCompiler
    --         >>= loadAndApplyTemplate "templates/data.html" defaultContext
    --         >>= relativizeUrls 

    -- match "assignments/*" $ do
    --     route $ setExtension "html"
    --     compile $ pandocCompilerS
    --         >>= loadAndApplyTemplate "templates/default.html" postCtx
    --         >>= relativizeUrls

    -- Archive (unlisted)
    match "archive/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompilerS
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

    -- TODO: match "projects/*" $ do

    -- Tags pages (posts by tag)
    tagsRules tags $ \tag pattern -> do
        route   idRoute
        compile $ do
          posts <- recentFirst =<< loadAll pattern
          let postsCtx =
                  constField "title" ("Posts tagged #" <> tag) <> 
                  listField "posts" postCtx (pure posts) <>
                  constField "tag" tag <>
                  defaultContext
          makeItem ""
           >>= loadAndApplyTemplate "templates/posts-tagged-x.html" postsCtx
           >>= loadAndApplyTemplate "templates/default.html" postsCtx
           >>= relativizeUrls

    -- Posts
    match "posts/**" $ do
        route $ setExtension "html"
        compile $ pandocCompilerS
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    -- Blog page (posts index)
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/**"
            let postsCtx =
                    listField "posts" postCtx (pure posts) <>
                    -- listField "tags" (field "tag" (pure . itemBody)) (mapM (makeItem . fst) $ tagsMap tags) <>
                    tagCloudFieldWith "tag-cloud" renderTagCloudLink mconcat 80 125 tags <>
                    constField "title" "Romes' Post Library" <>
                    -- constField "description" "Musings" <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/all-posts.html" postsCtx
                >>= loadAndApplyTemplate "templates/default.html" postsCtx
                >>= relativizeUrls

    -- Main page
    match "index.html" $ do
        route idRoute
        compile $ do

            posts <- recentFirst =<< loadAll "posts/**"
            posts_metadata <- mapM (getMetadata . itemIdentifier) posts
            let posts_with_preview = catMaybes $ zipWith (\meta p -> (itemIdentifier p,) <$> lookupString "preview" meta) posts_metadata posts
                indexCtx = listField "posts" postCtx (pure $ take 8 posts)
                            <> headField "latest-preview" (map snd posts_with_preview)
                            <> headField "latest-preview-desc" (mapMaybe (lookupString "preview-desc") posts_metadata)
                            <> headField "latest-preview-url" (map ((-<.> "html") . toFilePath . fst) posts_with_preview)
                            <> tagsFieldWith (const $ pure $ map fst $ tagsMap tags) renderLink mconcat "tags" tags
                            -- <> tagCloudFieldWith "tag-cloud" renderTagCloudLink mconcat 80 125 tags -- for now, no tag cloud.
                            <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    -- RSS Feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
          posts <- recentFirst =<< loadAll "posts/**"
          -- All our posts have a description, so no snapshots are needed
          renderRss feedConfiguration postCtx posts

    -- Atom Feed
    create ["atom.xml"] $ do
        route idRoute
        compile $ do
          posts <- recentFirst =<< loadAll "posts/**"
          -- All our posts have a description, so no snapshots are needed
          renderAtom feedConfiguration postCtx posts

--------------------------------------------------------------------------------

postCtx' :: Tags -> Context String
postCtx' tags = dateField "date" "%b %e, %Y" <>
               tagsListField "tags" tags <>
               defaultContext

tagsListField :: String -> Tags -> Context a
tagsListField = tagsFieldWith getTags renderLink mconcat

renderLink :: String -> Maybe FilePath -> Maybe H.Html
renderLink tag Nothing      = Just $ do
  H.li ! class_ ("tag-" <> fromString tag) $ do
     "#"
     H.a ! href "/"
         $ toHtml tag
renderLink tag (Just url) = Just $ do
  H.li ! class_ ("tag-" <> fromString tag) $ do
     "#"
     H.a ! href ("/" <> toValue url)
         $ toHtml tag

-- | Styled pandoc compiler with emoji support and much more!
pandocCompilerS :: Compiler (Item String)
pandocCompilerS =
    pandocCompilerWith
        defaultHakyllReaderOptions
            { readerExtensions = enableExtension Ext_tex_math_dollars $
                                 enableExtension Ext_tex_math_double_backslash $
                                 enableExtension Ext_emoji $
                                 enableExtension Ext_inline_notes $
                                 enableExtension Ext_footnotes $
                                   readerExtensions defaultHakyllReaderOptions
            }
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

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Romes' Blog RSS Feed"
    , feedDescription = "Romes' Blog RSS Feed"
    , feedAuthorName  = "Rodrigo Mesquita"
    , feedAuthorEmail = "rodrigo.m.mesquita@gmail.com"
    , feedRoot        = "http://alt-romes.github.io"
    }

renderTagCloudLink :: Double -> Double -> String -> String -> Int -> Int -> Int -> String
renderTagCloudLink minSize maxSize tag url count min' max' =
    -- Inlined from Hakyll
    -- Show the relative size of one 'count' in percent
    let diff     = 1 + fromIntegral max' - fromIntegral min'
        relative = (fromIntegral count - fromIntegral min') / diff
        size     = floor $ minSize + relative * (maxSize - minSize) :: Int
    in renderHtml $
        H.li ! class_ ("tag-" <> fromString tag) $ do
          "#"
          H.a ! style (toValue $ "font-size: " ++ show size ++ "%")
              ! href (toValue url)
              $ toHtml tag

----- Utils -------------------------------------------------------------------

headField :: String -> [String] -> Context a
headField _ [] = mempty
headField s (x:_) = constField s x


