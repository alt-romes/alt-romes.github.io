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


---- CMarkGFM ------------
mdtoNode :: T.Text -> Node
mdtoNode = commonmarkToNode [optUnsafe] [extStrikethrough]

nodetoHtml :: Node -> T.Text
nodetoHtml = nodeToHtml [optUnsafe] [extStrikethrough]

editNodeM :: (Monad m, Applicative m) => (Node -> m Node) -> Node -> m Node
editNodeM f (Node x nt nodes) = f . Node x nt =<< mapM (editNodeM f) nodes

editNode :: (Node -> Node) -> Node -> Node
editNode f n = runIdentity (editNodeM (return . f) n)



---- Haskeline -----------

-- Monad to get a sequence of valid user input
-- newtype ValidInput a = ValidInput { unValidInput :: InputT (MaybeT IO) a }

-- instance Functor ValidInput where
--     fmap f = ValidInput . fmap f . unValidInput

-- instance Applicative ValidInput where
--     pure = ValidInput . pure
--     a <*> b = ValidInput (unValidInput a <*> unValidInput b)

-- instance MonadIO ValidInput where
--     liftIO = ValidInput . liftIO

-- instance Monad ValidInput where
--     return = ValidInput . return
--     a >>= b = do
--         x <- liftIO $ runMaybeT $ runInputT defaultSettings $ unValidInput a
--         case x of
--           Nothing -> undefined -- ValidInput (liftIO $ return Nothing);
--           Just y -> b x


getUserInput :: (MonadIO m, MonadMask m) => String -> String -> InputT m (Maybe String)
getUserInput lin prompt = do
    outputStrLn lin
    getInputLine prompt
