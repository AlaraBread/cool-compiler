-- Defines the annotated COOL AST as Haskell types.
-- See https://kelloggm.github.io/martinjkellogg.com/teaching/cs485-sp25/projects/pa2.html#the-cl-ast-file-format.
module InputIr where

import Data.Foldable (minimumBy)
import Data.Int
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Util

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
type Expr = Lined ExprWithoutLine

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
  | IntegerConstant !Int
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
data LetBinding = LetBinding {letBindingName :: !Identifier, letBindingType' :: !Type, letBindingRhs :: !(Maybe (Typed Expr))}
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

-- Utility functions

isChild :: ParentMap -> Type -> Type -> Bool
isChild _ (Type "Object") (Type "Object") = True
isChild _ (Type "Object") _ = False
isChild pMap child parent = child == parent || (pMap Map.! child) == parent

-- Select the lowest parent of child out of the list. The types in the list need
-- to be distinct. If none of the types are parents, return Nothing.
pickLowestParent :: ParentMap -> Type -> [Type] -> Maybe Type
pickLowestParent pMap child parentCandidates = do
  let parents = filter (isChild pMap child) parentCandidates
  -- bail out early if we have no parents
  _ <- listToMaybe parents

  -- pick the lowest parent. note the parents are guaranteed to be in one branch
  -- of the inheritance hierarchy. Therefore, isChild forms a (complete) order
  -- on parents.
  let order t1 t2
        | t1 == t2 = EQ
        | isChild pMap t1 t2 = LT
        | otherwise = GT
  pure $ minimumBy order parents

-- Pick the lowest parent out of the provided list for all existing types.
pickLowestParents :: ClassMap -> ParentMap -> [Type] -> Map.Map Type (Maybe Type)
pickLowestParents cMap pMap parentCandidates =
  Map.mapWithKey (\k _ -> pickLowestParent pMap k parentCandidates) cMap
