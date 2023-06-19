{-# LANGUAGE
    OverloadedStrings,
    BlockArguments
  #-}
import Prelude hiding ((**))
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as T
import Clay

link, link_active :: Color
link = "#7FB4CA"
link_active = "#E82424"

-- | Generate CSS for a specific tag type from name and color
tag :: Text -> Color -> Css
tag name col = ".tags" ? do
  let tagname = element (".tag-" <> name)
  tagname ? do
    important (color col)
    before &
      background col
    a <?
      important (color col)
    -- a # ":active" <?
    --   important (color link_active)

stylesheet :: Css
stylesheet = do
  tag "haskell" "#8f4e8b"
  tag "vim" "#007f00"
  tag "beginner" "#98BB6C"
  tag "frp" "#FF9E3B"
  tag "graphics" "#E82424"
  tag "game-engine" "#16161D"


-- Output the compact version
main :: IO ()
-- main = putCss stylesheet
main = T.putStr $ renderWith compact [] stylesheet
