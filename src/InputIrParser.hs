module InputIrParser where

import Control.Monad (join)
import Control.Monad.State
import Data.Int
import qualified Data.Map.Strict as Map
import InputIr
import Util

-- Our top level parse function. This is the only thing we have to export;
-- everything else is an implementation detail.
parse :: String -> InputIr
parse input =
  evalState
    ( InputIr
        <$> parseClassMap
        <*> parseImplementationMap
        <*> parseParentMap
        <*> parseAst
    )
    $ lines input

-- A reasonable definition of a parser. We take a list of lines, parse out an a,
-- and return the a and the rest of the lines.
type Parser a = State [String] a

parseClassMap :: Parser ClassMap
parseClassMap =
  parseLine >> Map.fromList <$> parseList parseClassMapEntry

parseClassMapEntry :: Parser (Type, [Attribute])
parseClassMapEntry = do
  type' <- parseType
  let parseHasInitializer = ("initializer" ==) <$> parseLine
  attrs <- parseList (parseHasInitializer >>= parseAttribute)
  pure (type', attrs)

parseParentMap :: Parser ParentMap
parseParentMap =
  parseLine >> Map.fromList <$> parseList parseParentMapEntry

parseParentMapEntry :: Parser (Type, Type)
parseParentMapEntry = do
  parent <- parseType
  child <- parseType
  pure (parent, child)

parseImplementationMap :: Parser ImplementationMap
parseImplementationMap =
  parseLine >> Map.fromList <$> parseList parseImplementationMapEntries

parseImplementationMapEntries :: Parser (Type, [ImplementationMapEntry Method])
parseImplementationMapEntries = do
  class' <- parseType
  methods <- parseList (parseImplementationMapMethod class')
  -- add a placeholder method for the constructor
  let constructor =
        LocalImpl $
          Method
            { methodName = Identifier 0 ".new",
              methodFormals = [],
              methodBody = Typed (Type "INTERNAL") $ Lined 0 Constructor
            }
  pure (class', constructor : methods)

parseAst :: Parser Ast
parseAst = parseList parseClass

-- The "interesting" parsers (AST nodes)
parseClass :: Parser Class
parseClass = do
  identifier <- parseIdentifier
  classType <- parseLine
  -- We leave the case unexhaustive, because if it fails that means the input to
  -- the program is invalid, so crashing is acceptable. We do this everywhere it
  -- is relevant.
  case classType of
    "no_inherits" -> Class identifier Nothing <$> parseList parseFeature
    "inherits" -> Class identifier <$> (Just <$> parseIdentifier) <*> parseList parseFeature

parseFeature :: Parser Feature
parseFeature = do
  featureType <- parseLine
  case featureType of
    "attribute_no_init" -> AttributeF <$> parseAttribute False
    "attribute_init" -> AttributeF <$> parseAttribute True
    "method" -> MethodF <$> parseMethod

parseAttribute :: Bool -> Parser Attribute
parseAttribute initialized =
  if initialized
    then Attribute <$> (Identifier 0 <$> parseLine) <*> parseType <*> (Just <$> parseExpr)
    else Attribute <$> (Identifier 0 <$> parseLine) <*> parseType <*> pure Nothing

parseMethod :: Parser Method
parseMethod = Method <$> parseIdentifier <*> parseList (parseFormal True) <*> parseExpr

parseImplementationMapMethod :: Type -> Parser (ImplementationMapEntry Method)
parseImplementationMapMethod enclosingType = do
  -- I spent like 5 hours debugging this being a parseIdentifier. I need a hug.
  -- Or a better programming language. Or a lack of a skill issue.
  name <- Identifier 0 <$> parseLine
  formals <- parseList (parseFormal False)
  implementer <- parseType

  m <- Method name formals <$> parseExpr

  pure $
    if enclosingType == implementer
      then LocalImpl m
      else ParentImpl implementer $ lexeme name

parseFormal :: Bool -> Parser Formal
parseFormal typed = do
  name <- Identifier 0 <$> parseLine
  type' <-
    if typed
      then Just <$> parseType
      else pure Nothing
  pure $ Formal name type'

-- You would think this is the scary one, but no.
parseExpr :: Parser (Typed Expr)
parseExpr = do
  line' <- parseInt
  e <- parseExprWithoutLine
  pure $ Lined line' <$> e

-- This is the scary one. Realistically, it is not too bad, just a tad tedious.
-- You could do something more clever here, especially around binary operators,
-- but it is more trouble than it is worth. Honestly, the monadic nature of this
-- makes this much nicer than it could have been.
parseExprWithoutLine :: Parser (Typed ExprWithoutLine)
parseExprWithoutLine = do
  type' <- parseType
  exprType <- parseLine

  expr <- case exprType of
    "assign" -> Assign <$> parseIdentifier <*> parseExpr
    "dynamic_dispatch" -> DynamicDispatch <$> parseExpr <*> parseIdentifier <*> parseList parseExpr
    "static_dispatch" -> StaticDispatch <$> parseExpr <*> parseIdentifier <*> parseIdentifier <*> parseList parseExpr
    "self_dispatch" -> SelfDispatch <$> parseIdentifier <*> parseList parseExpr
    "if" -> If <$> parseExpr <*> parseExpr <*> parseExpr
    "while" -> While <$> parseExpr <*> parseExpr
    "block" -> Block <$> parseList parseExpr
    "new" -> New <$> parseIdentifier
    "isvoid" -> IsVoid <$> parseExpr
    "plus" -> Plus <$> parseExpr <*> parseExpr
    "minus" -> Minus <$> parseExpr <*> parseExpr
    "times" -> Times <$> parseExpr <*> parseExpr
    "divide" -> Divide <$> parseExpr <*> parseExpr
    "lt" -> LessThan <$> parseExpr <*> parseExpr
    "le" -> LessThanOrEqualTo <$> parseExpr <*> parseExpr
    "eq" -> Equal <$> parseExpr <*> parseExpr
    "not" -> Not <$> parseExpr
    "negate" -> Negate <$> parseExpr
    "integer" -> IntegerConstant . fromIntegral <$> parseInt
    "string" -> StringConstant <$> parseLine
    "identifier" -> Variable <$> parseIdentifier
    "true" -> pure $ BooleanConstant True
    "false" -> pure $ BooleanConstant False
    "let" -> Let <$> parseList parseLetBinding <*> parseExpr
    "case" -> Case <$> parseExpr <*> parseList parseCaseElement
    "internal" -> do
      kind <- parseLine
      pure $ case kind of
        "IO.in_int" -> InputInternal IOInInt
        "IO.in_string" -> InputInternal IOInString
        "IO.out_int" -> InputInternal IOOutInt
        "IO.out_string" -> InputInternal IOOutString
        "Object.abort" -> InputInternal ObjectAbort
        "Object.copy" -> InputInternal ObjectCopy
        "Object.type_name" -> InputInternal ObjectTypeName
        "String.concat" -> InputInternal StringConcat
        "String.length" -> InputInternal StringLength
        "String.substr" -> InputInternal StringSubstr

  pure $ Typed type' expr

-- Parses a single let binding.
parseLetBinding :: Parser LetBinding
parseLetBinding = do
  bindingType <- parseLine
  case bindingType of
    "let_binding_no_init" -> LetBinding <$> parseIdentifier <*> (identifierToType <$> parseIdentifier) <*> pure Nothing
    "let_binding_init" -> LetBinding <$> parseIdentifier <*> (identifierToType <$> parseIdentifier) <*> (Just <$> parseExpr)

-- Parses a single case element.
parseCaseElement :: Parser CaseElement
parseCaseElement = CaseElement <$> parseIdentifier <*> (identifierToType <$> parseIdentifier) <*> parseExpr

identifierToType :: Identifier -> Type
identifierToType (Identifier _ lexeme) = Type lexeme

parseIdentifier :: Parser Identifier
parseIdentifier = Identifier <$> parseInt <*> parseLine

-- Time for some quite trivial (but very useful!) parsers.

-- Sometimes a type is worth a thousand words.
-- The fact you can write this function like this *feels* magical.
parseList :: Parser a -> Parser [a]
parseList p = parseInt >>= (sequenceA <$> flip replicate p)

parseInt :: Parser Int
parseInt = read <$> parseLine

parseType :: Parser Type
parseType = Type <$> parseLine

-- Literally extracts the first line of the state.
parseLine :: Parser String
parseLine = do
  input <- get
  put $ tail input
  pure $ head input
