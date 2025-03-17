-- Defines the annotated COOL AST as Haskell types.
-- See https://kelloggm.github.io/martinjkellogg.com/teaching/cs485-sp25/projects/pa2.html#the-cl-ast-file-format.
module InputIr where

import Data.Int
import qualified Data.Map.Strict as Map

data InputIr = InputIr !ClassMap !ImplementationMap !ParentMap !Ast
  deriving (Show)

-- Represents a class. We make the choice to represent classes with/without a
-- superclass with Maybe; we make similar choices throughout for optional
-- features.

type ClassMap = Map.Map Type [Attribute]

-- The Type in the right hand side is the place where the method is implemented
type ImplementationMap = Map.Map Type [(Type, Method)]

-- Child to parent
type ParentMap = Map.Map Type Type

type Ast = [Class]

data ImplementationMapEntry = ImplementationMapEntry !Identifier ![Method]
  deriving (Show)

-- Child then parent
data ParentMapEntry = ParentMapEntry !Identifier !Identifier

data Class = Class
  { className :: !Identifier,
    superClassName :: !(Maybe Identifier),
    classFeatures :: ![Feature]
  }
  deriving (Show)

-- Represents a feature of a class. Lifted directly from the documentation.
data Feature
  = AttributeF !Attribute
  | MethodF !Method
  deriving (Show)

data Attribute = Attribute {attrName :: !Identifier, attrType :: !Type, attrRhs :: !(Maybe (Typed Expr))}
  deriving (Show)

data Method = Method {methodName :: !Identifier, methodFormals :: ![Formal], methodBody :: !(Typed Expr)}
  deriving (Show)

-- Represents a formal parameter. Lifted directly from the documentation. Note
-- that in the implementation map, Formals do not have a type. For some reason.
data Formal = Formal {formalName :: !Identifier, formalType :: !(Maybe Type)}
  deriving (Show)

-- Wraps an ExprWithoutLine with a line number. This way, we can treat line
-- numbers uniformly among all expression types (and avoid repetition).
data Expr
  = Expr !Int !ExprWithoutLine
  deriving (Show)

-- We pretty directly match the documentation on expressions, in form (though
-- not necessarily in field names). For brevity, we do not use records for
-- expressions with sufficiently trivial structure (most unary/binary
-- operations).
data ExprWithoutLine
  = Assign !Identifier !(Typed Expr)
  | DynamicDispatch {dynamicDispatchLhs :: !(Typed Expr), dynamicDispatchMethod :: !Identifier, dynamicDispachArgs :: ![Typed Expr]}
  | StaticDispatch {staticDispatchLhs :: !(Typed Expr), dynamicDispatchType :: !Identifier, dynamicDispatchMethod :: !Identifier, dynamicDispatchArgs :: ![Typed Expr]}
  | SelfDispatch {selfDispatchMethod :: !Identifier, selfDispatchArgs :: ![Typed Expr]}
  | If {ifPredicate :: !(Typed Expr), trueBody :: !(Typed Expr), falseBody :: !(Typed Expr)}
  | While {whilePredicate :: !(Typed Expr), whileBody :: !(Typed Expr)}
  | Block ![Typed Expr]
  | New !Identifier
  | IsVoid !(Typed Expr)
  | Plus !(Typed Expr) !(Typed Expr)
  | Minus !(Typed Expr) !(Typed Expr)
  | Times !(Typed Expr) !(Typed Expr)
  | Divide !(Typed Expr) !(Typed Expr)
  | LessThan !(Typed Expr) !(Typed Expr)
  | LessThanOrEqualTo !(Typed Expr) !(Typed Expr)
  | Equal !(Typed Expr) !(Typed Expr)
  | Not !(Typed Expr)
  | Negate !(Typed Expr)
  | IntegerConstant !Data.Int.Int32
  | StringConstant !String
  | Variable !Identifier
  | BooleanConstant !Bool
  | Let ![LetBinding] !(Typed Expr)
  | Case !(Typed Expr) ![CaseElement]
  | -- internal expressions
    IOInInt
  | IOInString
  | IOOutInt
  | IOOutString
  | ObjectAbort
  | ObjectCopy
  | ObjectTypeName
  | StringConcat
  | StringLength
  | StringSubstr
  deriving (Show)

-- Represents a binding in a let binding.
data LetBinding = LetBinding {letBindingName :: !Identifier, letBindingType' :: !Identifier, letBindingRhs :: !(Maybe (Typed Expr))}
  deriving (Show)

-- Represents a case element.
data CaseElement = CaseElement {caseElementVariable :: !Identifier, caseElementType :: !Identifier, caseElementBody :: !(Typed Expr)}
  deriving (Show)

-- Type type.
data Typed a = Typed {type' :: !Type, item :: !a}
  deriving (Show)

instance Functor Typed where
  fmap f (Typed type' item) = Typed type' $ f item

newtype Type = Type String
  deriving (Eq, Ord, Show)

-- Represents an identifier; fairly self explanatory
data Identifier = Identifier
  { line :: !Int,
    lexeme :: !String
  }
  deriving (Show)
