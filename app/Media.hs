{-# LANGUAGE OverloadedStrings #-}
module Media where

import qualified Data.Text as T
import Data.Time
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Control.Monad

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
                    mapM_ ((td ! class_ "green") . toHtml) attr_names
                tr $
                    mapM_ ((td ! class_ "grey") . toHtml . flip T.replicate "-" . T.length) attr_names
            tbody $
                mapM_ mediaRow (m:ms)

    mediaToHtml :: [a] -> Html
    mediaToHtml ms = docTypeHtml $ do
        H.head $ do
            meta ! httpEquiv "content-type" ! content "text/html; charset=utf8"
            H.title "romes"
            link ! rel "stylesheet" ! href "style.css" ! media "all" ! type_ "text/css"
        body $ do
            mediaTable ms

    getDate :: a -> Day

    -- TODO: Filter for last 30 days instead
    filterMonth :: Day -> [a] -> [a]
    filterMonth today ms = filter thisMonthF ms
        where
            thisMonthF m = (\(a,b,_) (a',b',_) -> a == a' && b == b') (toGregorian $ getDate m) (toGregorian today)

