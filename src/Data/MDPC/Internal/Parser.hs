module Data.MDPC.Internal.Parser
  (
    programP
  ) where

-- import           Data.MDPC.Internal.Parser.Keywords (kwBegin, kwEnd)
import           Data.MDPC.Internal.Parser.Utils (caseInsensitiveString)
import           Data.MDPC.Internal.Syntax       (Expression (..), Literal (..),
                                                  Program (..), Statement (..),
                                                  SubroutineArg (..),
                                                  SubroutineDeclaration (..),
                                                  SubroutineSignature (..))
import           Data.MDPC.Internal.Types        (ForLoopDirection (..),
                                                  Identifier (..))
import           Text.Megaparsec
import           Text.Megaparsec.Char


-----------------------------------------------------------------------------------------
  -- UNIT PARSER
-----------------------------------------------------------------------------------------

programP :: (MonadParsec e s f, Token s ~ Char) => f Program
programP = undefined

-----------------------------------------------------------------------------------------
  -- SUBROUTINE PARSER
-----------------------------------------------------------------------------------------

subroutineArgP :: (MonadParsec e s f, Token s ~ Char) => f SubroutineArg
subroutineArgP = undefined

subroutineDeclarationP :: (MonadParsec e s f, Token s ~ Char) => f SubroutineDeclaration
subroutineDeclarationP = undefined

subroutineSignatureP :: (MonadParsec e s f, Token s ~ Char) => f SubroutineSignature
subroutineSignatureP = undefined

-----------------------------------------------------------------------------------------
  -- STATEMENT PARSER
-----------------------------------------------------------------------------------------

statementP :: (MonadParsec e s f, Token s ~ Char) => f Statement
statementP = undefined

forLoopDirectionP :: (MonadParsec e s f, Token s ~ Char) => f ForLoopDirection
forLoopDirectionP = undefined

-----------------------------------------------------------------------------------------
  -- EXPRESSION PARSER
-----------------------------------------------------------------------------------------

expressionP :: (MonadParsec e s f, Token s ~ Char) => f Expression
expressionP = undefined

-----------------------------------------------------------------------------------------
  -- LITERAL PARSER
-----------------------------------------------------------------------------------------

literalP :: (MonadParsec e s f, Token s ~ Char) => f Literal
literalP = undefined

-----------------------------------------------------------------------------------------
  -- IDENTIFIER PARSER
-----------------------------------------------------------------------------------------

identifierP :: (MonadParsec e s f, Token s ~ Char) => f Identifier
identifierP = do
  first <- letterChar
  rest  <- many (alphaNumChar <|> digitChar <|> char '_')
  pure $ Identifier (first : rest)

