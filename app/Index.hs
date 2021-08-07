{-# LANGUAGE OverloadedStrings #-}
module Index where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

indexHtml :: Html
indexHtml = docTypeHtml $ do
    H.head $ do
        meta ! httpEquiv "content-type" ! content "text/html; charset=utf8"
        H.title "romes"
        link ! rel "stylesheet" ! href "style.css" ! media "all" ! type_ "text/css"
    body ! class_ "text-container" $ do
        h1 "romes" 

        p "Lorem ipsum dolor sit amet, malis incorrupte et qui, modo alienum et eam. Stet vivendum imperdiet eu mei, te has clita graecis. An mea posse atomorum. Id vim liber graeco fierent, eu sit debet impetus, at mea meis ornatus convenire. In aeque utamur feugiat his, tacimates repudiare mel ut."

        p "占だよべわ協豊ツサ少3図ヨホケ議覧ぎみの社手っレうも靖施ざす表60話せ盗美ツウヒユ納査ミ目問27条必弱業ヒウ枚持排撲隣枠げ。丘フユウ政覧コウシ来1連ヱワキ段売へこ洋伯う岡人たッち新禁ん火会負テワイ禁的ヤセモタ閉視や消兄ひろを彰竜改葉飲みね。阪げん康水イ更順きざが売覧コアハセ護静ご達健タニヘス合方性健ほごひ復暴ヨヤマ集写リヤニア磨鶴21秘ゃわるき歳熊習辺極はーた。"

        p "Лорем ипсум долор сит амет, сит харум цонсулату цонсецтетуер цу. Харум алияуандо вих но, ессе лаореет патриояуе хис ин. Еам ан постеа цоммодо. Мутат цонцептам яуо не, ет еос нисл толлит облияуе. Ид волуптуа десерунт абхорреант хис. Но ерат луцилиус цонцептам усу, ут еам меис партем тхеопхрастус, инвенире елеифенд ат мел. Но при поссим витуперата, ан фацер инвенире сит."

