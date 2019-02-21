module Data.MDPC.Internal.Syntax
  (
    Literal (..)
  , Expression (..)
  , Statement (..)
  , SubroutineSignature (..)
  , SubroutineArg (..)
  , SubroutineDeclaration (..)
  , Program (..)
  ) where

import           Data.MDPC.Internal.Types (ForLoopDirection, Identifier, Type)
import           GHC.Generics             (Generic (..))

data Literal = IntegerLit Int
             | CharacterLit Char
             | BooleanLit Bool
             | StringLit String
             | RealLit Double
  deriving (Eq, Show, Generic)

-- | A value can be either a raw literal or a variable or the result of an expression.
--
data Value = LitValue Literal     -- ^ "Lorem ipsum"
           | VarValue Identifier  -- ^ myVariable
           | ExprValue Expression -- ^ fact(3) * (sqrt(16) + 27 div 9)
  deriving (Eq, Show, Generic)

-- | An expression is a non-empty sequence of operations that returns a value.
--
data Expression = ValueExpr Value                       -- ^ "Lorem Ipsum"
                | UnaryExpr Expression                  -- ^ -42
                | BinaryExpr Expression Expression      -- ^ a + b
                | SubroutineCallExpr Identifier [Value] -- ^ gcd(228, 71)
  deriving (Eq, Show, Generic)

-- | A statement is a non-empty sequence of operations that returns no values.
--
data Statement = AssignStmt Identifier Value                                     -- ^ a := b;
               | SubroutineCallStmt Identifier [Value]                           -- ^ printLn("Hello!");
               | ForLoopStmt Identifier Value ForLoopDirection Value [Statement] -- ^ for I := 1 to 10 { ... }
  deriving (Eq, Show, Generic)

data SubroutineSignature = FunctionSig Identifier [SubroutineArg] Type -- ^ function foo(x : Integer, var y : Real) : Real;
                         | ProcedureSig Identifier [SubroutineArg]     -- ^ procedure bar(x : Integer, var y : Real);
  deriving (Eq, Show, Generic)

-- | Argument passed to a function or procedure.
-- [var] x : Integer
data SubroutineArg = SubroutineArg Bool Identifier Type
  deriving (Eq, Show, Generic)

data VarDeclaration = VarDeclaration [Identifier] Type  -- ^ Var X, Y, Z : Integet;
  deriving (Eq, Show, Generic)

-- | Declaration of a function or a procedure
-- function foo(u : Integer);
-- var x : Integer;
--     y, z : Real;
-- begin
--   ... statements ...
-- end;
data SubroutineDeclaration = SubroutineDeclaration SubroutineSignature [VarDeclaration] [Statement]
  deriving (Eq, Show, Generic)

-- |
data Program = Program Identifier [Identifier] [SubroutineDeclaration]
  deriving (Eq, Show, Generic)

