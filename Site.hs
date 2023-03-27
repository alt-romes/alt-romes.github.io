--------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.String
import Data.List (intersperse)
import Data.Map (Map, singleton)
import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.Html5.Attributes (href, class_, style)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 (toHtml, (!), toValue)
import qualified Text.Blaze.Html5 as H

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

    tagsRules tags $ \tag pattern -> do
        route   idRoute
        compile $ do
          posts <- recentFirst =<< loadAll pattern
          let postsCtx =
                  listField "posts" postCtx (pure posts) <>
                  constField "tag" tag <>
                  defaultContext
          makeItem ""
           >>= loadAndApplyTemplate "templates/tags.html" postsCtx
           >>= loadAndApplyTemplate "templates/default.html" postsCtx
           >>= relativizeUrls

    match "posts/**" $ do
        route $ setExtension "html"
        compile $ pandocCompilerS
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/**"
            let postsCtx =
                    listField "posts" postCtx (pure posts) <>
                    constField "title" "Romes' Post Library" <>
                    -- constField "description" "Musings" <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" postsCtx
                >>= loadAndApplyTemplate "templates/default.html" postsCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do

            posts <- recentFirst =<< loadAll "posts/**"
            let indexCtx = listField "posts" postCtx (pure $ take 5 posts)
                            <> tagsFieldWith (const $ pure $ map fst $ tagsMap tags) renderLink mconcat "tags" tags
                            -- <> tagCloudFieldWith "tag-cloud" renderTagCloudLink mconcat 80 125 tags -- for now, no tag cloud.
                            <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls


--------------------------------------------------------------------------------
postCtx' :: Tags -> Context String
postCtx' tags = dateField "date" "%B %e, %Y" <>
               tagsListField "tags" tags <>
               defaultContext
  where
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
