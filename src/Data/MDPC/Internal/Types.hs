module Data.MDPC.Internal.Types
  (
    Identifier (..)
  , Type
  , ForLoopDirection (..)
  ) where

import           Data.Char     (toLower)
import           Data.Coerce   (coerce)
import           Data.Function (on)
import           GHC.Generics  (Generic (..))

newtype Identifier = Identifier String
  deriving (Show, Generic)

instance Eq Identifier where
  (==) = (==) `on` map toLower . coerce

type Type = Identifier

data ForLoopDirection = ForTo | ForDownTo
  deriving (Eq, Show, Generic)

