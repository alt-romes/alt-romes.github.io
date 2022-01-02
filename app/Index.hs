{-# LANGUAGE OverloadedStrings #-}
module Index where

import CMarkGFM
import Debug.Trace
import Data.Time
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text as BRT
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import Util
import Media
import Movies
import Animes
import Page

data Index = Index Day [Movie] T.Text

makeTablesUrl :: Day -> [Movie] -> Node -> Node
makeTablesUrl today mvs = editNode editImages
    where
        editImages (Node x i@(IMAGE _ _) nodes) = Node x (makeTableUrl' i) nodes
        editImages x = x

        makeTableUrl' (IMAGE url title) = HTML_INLINE $ TL.toStrict $ BRT.renderHtml $ media' url title

        media' :: T.Text -> T.Text -> Html
        media' url title
          | "romes://movies" == url && not (null mvs) = figure $ do
              mediaTable mvs
              figcaption $ toHtml title
          | "romes://movies-month" == url && any ((>= 8) . Movies.rating) (filterMonth today mvs) = figure $ do
              mediaTable $ filter ((>= 8) . Movies.rating) $ filterMonth today mvs
              figcaption $ toHtml title
          | otherwise = H.span ""


instance Page Index where
    makeMain (Index today movies content) =
        preEscapedToHtml $ nodetoHtml $ makeTablesUrl today movies (mdtoNode content)

    pageDepth _ = 0
