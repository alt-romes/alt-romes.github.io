{-# LANGUAGE OverloadedStrings #-}
module Media where

import qualified Data.Text as T
import Data.Time
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Control.Monad

import Page

class Media a where
    mediaAttrNames :: a -> [T.Text]
    mediaAttrs :: a -> [Html]

    mediaRow :: a -> Html
    mediaRow m =
        let attrs = mediaAttrs m in
        tr $
            mapM_ td attrs

    mediaTable :: [a] -> Html
    mediaTable [] = H.section ! class_ "text-center" $ ""
    mediaTable (m:ms) =
        H.section ! class_ "text-center" $
        H.div ! class_ "table-border" $
        table $ do
            let attr_names = mediaAttrNames m
            thead $ do
                tr $
                    mapM_ ((td ! class_ "orange") . toHtml) attr_names
                tr $
                    mapM_ ((td ! class_ "grey") . toHtml . flip T.replicate "-" . T.length) attr_names
            tbody $
                mapM_ mediaRow (m:ms)

    getDate :: a -> Day

    -- TODO: Filter for last 30 days instead
    filterMonth :: Day -> [a] -> [a]
    filterMonth today = filter thisMonthF
        where
            thisMonthF m = (\(a,b,_) (a',b',_) -> a == a' && b == b') (toGregorian $ getDate m) (toGregorian today)


instance Media a => Page [a] where
    makeMain = mediaTable
    pageDepth _ = 0
    makePageMethod = makePageWithNav
