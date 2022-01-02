{-# LANGUAGE OverloadedStrings #-}
module Post where

import qualified Data.Text as T
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Data.List as L
import Control.Monad

import Util
import Page

data Post = Post String -- Post path
                 T.Text -- Post content

newtype Posts = Posts [Post]

instance Page Post where
    makeMain (Post _ content) = preEscapedToHtml $ mdtoHtml content
    pageDepth _ = 1

-- Posts index
instance Page Posts where
    makeMain (Posts posts) = do
        p "---"
        ul ! class_ "posts-index" $ forM_ posts $ \(Post postPath _) -> do
            li $ a ! href (stringValue $ "posts/" <> dropSuffix ".md" postPath <> ".html") $
                toHtml $ L.map (\x -> if x == '-' then ' ' else x) $ dropSuffix ".md" postPath
            br

    pageDepth _ = 0
