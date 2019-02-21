module Data.MDPC.Internal.Parser.Keywords
  (
    kwBegin
  , kwEnd
  , kwDo
  , kwProcedure
  , kwFunction
  ) where

import           Data.MDPC.Internal.Parser.Utils (caseInsensitiveString)
import           Text.Megaparsec

kwBegin :: (MonadParsec e s f, Token s ~ Char) => f String
kwBegin = caseInsensitiveString "begin"

kwEnd :: (MonadParsec e s f, Token s ~ Char) => f String
kwEnd = caseInsensitiveString "end"

kwDo :: (MonadParsec e s f, Token s ~ Char) => f String
kwDo = caseInsensitiveString "do"

kwProcedure :: (MonadParsec e s f, Token s ~ Char) => f String
kwProcedure = caseInsensitiveString "procedure"

kwFunction :: (MonadParsec e s f, Token s ~ Char) => f String
kwFunction = caseInsensitiveString "function"
