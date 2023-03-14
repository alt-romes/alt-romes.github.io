{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Projects where

import Data.Yaml as Y
import GHC.Generics
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Data.Text as T

import Util
import Page
import Media

data Project = Project {
    title :: Text,
    description :: Text,
    state :: Text,
    url :: Text
} deriving (Show, Eq, Generic)

instance ToJSON Project
instance FromJSON Project

newtype Projects = Projects [Project]

instance Page Projects where
    makeMain (Projects ps) = makeList ps
        where
        makeItem :: Project -> Html
        makeItem (Project title description state url) =
            p $ do
                a ! href (textValue url) ! class_ "orange" $ toHtml title
                -- toHtml (" [" <> state <> "]")
                blockquote ! class_ "description" $ toHtml description

        makeList :: [Project] -> Html
        makeList projects =
            H.div ! class_ "projects" $
            mapM_ makeItem projects


    pageDepth _ = 0
    makePageMethod = makePageWithNav
