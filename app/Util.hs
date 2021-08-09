{-# LANGUAGE OverloadedStrings #-}
module Util where

import CMarkGFM
import Control.Monad.Identity
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

mdtoNode :: T.Text -> Node
mdtoNode = commonmarkToNode [optUnsafe] [extStrikethrough]

nodetoHtml :: Node -> T.Text
nodetoHtml = nodeToHtml [optUnsafe] [extStrikethrough]

editNodeM :: (Monad m, Applicative m) => (Node -> m Node) -> Node -> m Node
editNodeM f (Node x nt nodes) = f . Node x nt =<< mapM (editNodeM f) nodes

editNode :: (Node -> Node) -> Node -> Node
editNode f n = runIdentity (editNodeM (return . f) n)
