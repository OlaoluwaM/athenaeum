{-# LANGUAGE DeriveTraversable #-}

module Helpers where

import Data.Text (Text)
import Data.UUID (UUID)
import System.Random (mkStdGen, random)
import TextShow (fromString, toText)

data V3 a = V3 a a a deriving (Eq, Show, Read, Ord, Functor, Foldable, Traversable)

tshow :: (Show a) => a -> Text
tshow = toText . fromString . show

-- Use this to generate a registration ID from an unregistered patron's idNumber to create their registration ID when they sign up
-- Is Idempotent
randomUUID :: Word -> UUID
randomUUID = fst . random . mkStdGen . fromInteger . toInteger

traverseTupleThree :: (Applicative f) => (a -> f b) -> (a, a, a) -> f (b, b, b)
traverseTupleThree f ~(a, b, c) = (,,) <$> f a <*> f b <*> f c
