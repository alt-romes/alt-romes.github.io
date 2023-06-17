{-# LANGUAGE
    OverloadedStrings,
    BlockArguments
  #-}
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as T
import Clay


-- | Generate CSS for a specific tag type from name and color
tag :: Text -> Color -> Css
tag name col = do
  let tagname = element (".tag-" <> name)
  tagname ? do
    important (color col)
    before &
      background col
    a # ":active" <?
      important (color col)


stylesheet :: Css
stylesheet = do
  tag "haskell" "#8f4e8b"
  tag "beginner" "#FFE5B4"
  tag "vim" "#007f00"
  tag "frp" orange
  tag "graphics" red
  tag "game-engine" black


-- Output the compact version
main :: IO ()
-- main = putCss stylesheet
main = T.putStr $ renderWith compact [] stylesheet
