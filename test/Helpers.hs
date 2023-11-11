module Helpers where

import Data.Text (Text)
import TextShow (fromString, toText)

tshow :: (Show a) => a -> Text
tshow = toText . fromString . show
