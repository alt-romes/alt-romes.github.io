{-# LANGUAGE OverloadedStrings #-}
module Index where

import CMarkGFM
import Debug.Trace
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text as BRT
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import Util
import Media
import Movies
import Animes

-- indexHtml :: Html
-- indexHtml = H.main $ do
--             p "Lorem ipsum dolor sit amet, malis incorrupte et qui, modo alienum et eam. Stet vivendum imperdiet eu mei, te has clita graecis. An mea posse atomorum. Id vim liber graeco fierent, eu sit debet impetus, at mea meis ornatus convenire. In aeque utamur feugiat his, tacimates repudiare mel ut."

--             p "占だよべわ協豊ツサ少3図ヨホケ議覧ぎみの社手っレうも靖施ざす表60話せ盗美ツウヒユ納査ミ目問27条必弱業ヒウ枚持排撲隣枠げ。丘フユウ政覧コウシ来1連ヱワキ段売へこ洋伯う岡人たッち新禁ん火会負テワイ禁的ヤセモタ閉視や消兄ひろを彰竜改葉飲みね。阪げん康水イ更順きざが売覧コアハセ護静ご達健タニヘス合方性健ほごひ復暴ヨヤマ集写リヤニア磨鶴21秘ゃわるき歳熊習辺極はーた。"

--             p "Лорем ипсум долор сит амет, сит харум цонсулату цонсецтетуер цу. Харум алияуандо вих но, ессе лаореет патриояуе хис ин. Еам ан постеа цоммодо. Мутат цонцептам яуо не, ет еос нисл толлит облияуе. Ид волуптуа десерунт абхорреант хис. Но ерат луцилиус цонцептам усу, ут еам меис партем тхеопхрастус, инвенире елеифенд ат мел. Но при поссим витуперата, ан фацер инвенире сит."



makeTablesUrl :: [Movie] -> [Anime] -> Node -> Node
makeTablesUrl mvs animes = editNode editImages
    where
        editImages (Node x i@(IMAGE _ _) nodes) = Node x (makeTableUrl' i) nodes
        editImages x = x

        makeTableUrl' (IMAGE url title) = HTML_INLINE $ TL.toStrict $ BRT.renderHtml $ figure $ do
            media' url
            figcaption $ toHtml title

        media' :: T.Text -> Html
        media' url
         | T.isInfixOf "movies" url = mediaTable $ take 5 mvs
         | T.isInfixOf "animes" url = mediaTable $ take 5 animes
         | otherwise = H.span ""
         -- T.isInfixOf "albums" url = mediaTable $ take 5 animes

            
            


indexHtml :: [Movie] -> [Anime] -> Node -> Html
indexHtml movies animes content = H.main $ do
    preEscapedToHtml $ nodetoHtml $ makeTablesUrl movies animes content
