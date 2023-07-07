module Helpers where

import Data.Text (Text)

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither errorEntity = maybe (Left errorEntity) Right
