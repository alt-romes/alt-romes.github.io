{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

import qualified Data.Text.Lazy.IO as T
import Clay

main :: IO ()
-- main = T.putStr $ renderWith compact style
main = putCss css


css :: Css
css = do
  body ? do
    margin nil nil nil nil
    display flex

