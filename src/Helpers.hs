module Helpers where

import Data.Text (Text)
import TextShow (fromString, toText)

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither errorEntity = maybe (Left errorEntity) Right

tshow :: (Show a) => a -> Text
tshow = toText . fromString . show
