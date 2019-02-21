module Data.MDPC.Internal.Parser.Utils
  (
    caseInsensitiveChar
  , caseInsensitiveString
  ) where

import           Data.Char            (toLower, toUpper)
import           Text.Megaparsec
import           Text.Megaparsec.Char

caseInsensitiveChar :: (MonadParsec e s f, Token s ~ Char) => Char -> f Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

caseInsensitiveString :: (MonadParsec e s f, Token s ~ Char) => String -> f String
caseInsensitiveString = mapM caseInsensitiveChar

