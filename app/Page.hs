{-# LANGUAGE UndecidableInstances, FlexibleInstances, OverloadedStrings #-}
module Page where

import qualified Data.ByteString.Lazy.Char8 as BL
import Text.Blaze.Html.Renderer.Utf8 as U
import qualified Data.Text as T
import Data.Time
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Control.Monad

class Page a where
    makeMain :: a -> Html
    pageDepth :: a -> Int
    makePageMethod :: a -> Html

    makePage :: a -> Html
    makePage page = docTypeHtml $ do
        H.head $ do
            meta ! httpEquiv "content-type" ! content "text/html; charset=utf8"
            H.title "romes"
            link ! rel "stylesheet" ! href "style.css" ! media "all" ! type_ "text/css"
        body $ makeMain page

    makePageWithNav :: a -> Html
    makePageWithNav page =  docTypeHtml $ do
        H.head $ do
            meta ! httpEquiv "content-type" ! content "text/html; charset=utf8"
            H.title "romes"
            link ! rel "stylesheet" ! relHref "style.css" ! media "all" ! type_ "text/css"
        body ! class_ "main-body" $ do
            H.div ! class_ "darker" $ H.span ""
            -- $ H.div ! class_ "dark" $ H.div ! class_ "opacity" $ do
            H.div ! class_ "main-container" $ do
                nav $ do
                    ul $ do
                        li ! class_  "logo" $ a ! relHref "index.html" $ "romes"
                        li $ a ! relHref "posts.html" $ "posts"
                        li "music"
                        li $ a ! href "https://github.com/alt-romes" ! target "_blank" $ "github"
                H.main $ makeMain page
        -- footer ! class_ "main-footer" $ do
        --     ul $ do
        --         li $ a ! relHref "index.html" $ "en"
        --         li $ a ! relHref "de/index.html" $ "de"

        where
            relHref :: AttributeValue -> Attribute
            relHref path = if pageDepth page > 0 then href (foldl (<>) "" (replicate (pageDepth page) "../") <> path) else href path

    writeHtmlPage :: FilePath -> a -> IO ()
    writeHtmlPage filePath page = BL.writeFile filePath $ U.renderHtml $ makePageMethod page
