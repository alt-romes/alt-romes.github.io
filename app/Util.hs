{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Util where

import CMarkGFM
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.Haskeline
import qualified Text.Blaze.Html5 as H


---- General -------------
dropSuffix :: String -> [a] -> [a]
dropSuffix = dropFromEnd . length

dropFromEnd :: Int -> [a] -> [a]
dropFromEnd i l = take (length l - i) l


---- CMarkGFM ------------
mdtoHtml :: T.Text -> T.Text
mdtoHtml = commonmarkToHtml [optUnsafe] [extStrikethrough]

mdtoNode :: T.Text -> Node
mdtoNode = commonmarkToNode [optUnsafe] [extStrikethrough]

nodetoHtml :: Node -> T.Text
nodetoHtml = nodeToHtml [optUnsafe] [extStrikethrough]

editNodeM :: (Monad m, Applicative m) => (Node -> m Node) -> Node -> m Node
editNodeM f (Node x nt nodes) = f . Node x nt =<< mapM (editNodeM f) nodes

editNode :: (Node -> Node) -> Node -> Node
editNode f n = runIdentity (editNodeM (return . f) n)


---- Haskeline -----------
getUserInput :: (MonadIO m, MonadMask m) => String -> String -> InputT m (Maybe String)
getUserInput lin prompt = do
    outputStrLn lin
    getInputLine prompt


---- Blaze-Html ----------
htmlMain = H.main -- re-export H.main to be useable in Main.hs without conflict and without needing to qualify the import there.
