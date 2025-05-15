{-# LANGUAGE NamedFieldPuns #-}

-- This is just /really/ fancy constant folding. I [ abi ] explicitly asked the
-- professor if "run the program at compile time" is a valid optimization, and
-- he said yes so :3.

module Interpreter where

import Control.Exception (evaluate)
import Control.Monad (when)
import Control.Monad.State
import Data.Foldable (find, traverse_)
import Data.Int (Int32)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import InputIr (Attribute (Attribute), CaseElement (CaseElement), ClassMap, Expr, ExprWithoutLine (..), Formal (formalName), Identifier (Identifier, lexeme), ImplementationMap, ImplementationMapEntry (..), InputIr (InputIr), Internal (..), LetBinding (..), Method (Method, methodBody, methodFormals, methodName), ParentMap, Typed (Typed), implementationMapEntryName)
import Util (Lined (Lined), Type (Type))

resolveImplementationMap :: ImplementationMap -> ImplementationMapEntry Method -> Method
resolveImplementationMap implementationMap entry = case entry of
  LocalImpl m -> m
  ParentImpl parent name ->
    let LocalImpl m =
          fromJust $
            find ((name ==) . implementationMapEntryName (lexeme . methodName)) $
              implementationMap Map.! parent
     in m

lookupImplementationMap :: ImplementationMap -> String -> String -> Method
lookupImplementationMap implementationMap typeName name =
  let entry =
        fromJust $
          find ((name ==) . implementationMapEntryName (lexeme . methodName)) $
            implementationMap Map.! Type typeName
   in resolveImplementationMap implementationMap entry

data Object = Object Type (Map.Map String Location) | IntObject Int32 | BoolObject Bool | StringObject String | VoidObject

type Location = Int

type Store = (Map.Map Location Object, Location)

type Environment = Map.Map String Location

data Success = RequiredInput | Success

type InterpreterState = StateT (Environment, Store, Success, String) Maybe

requireInput :: InterpreterState a
requireInput = do
  (a, b, _, _) <- get
  put (a, b, RequiredInput, "")
  fail ""

abort :: InterpreterState a
abort = do
  (a, b, c, output) <- get
  put (a, b, c, output ++ "abort\n")
  fail ""

runtimeError :: Int -> String -> InterpreterState a
runtimeError lineNumber msg = do
  (a, b, c, output) <- get
  put (a, b, c, output ++ "ERROR: " ++ show lineNumber ++ ": Exception: " ++ msg ++ "\\n")
  fail ""

interpret :: InputIr -> Maybe String
interpret (InputIr classMap implementationMap parentMap _) =
  do
    let main = lookupImplementationMap implementationMap "Main" "main"
    (_, (_, _, success, output)) <-
      runStateT
        ( do
            mainLoc <- runExpr classMap implementationMap parentMap 0 $ Typed (Type "Main") $ Lined 0 $ New (Identifier 0 "Main")
            runMethod classMap implementationMap parentMap mainLoc main
        )
        (Map.empty, (Map.empty, 0), Success, "")
    case success of
      RequiredInput -> Nothing
      Success -> Just output

assign :: Object -> Identifier -> Location -> InterpreterState ()
assign (Object _ attrEnv) (Identifier _ lhs) rhs = do
  (environment, (store, l), c, d) <- get
  let store' = Map.insert (fromMaybe (attrEnv Map.! lhs) $ Map.lookup lhs environment) (store Map.! rhs) store
  put (environment, (store', l), c, d)
  pure ()

lookupLocation :: Location -> InterpreterState Object
lookupLocation loc = do
  (_, (store, _), _, _) <- get
  pure $ store Map.! loc

lookupVariable :: String -> InterpreterState Object
lookupVariable name = do
  (env, _, _, _) <- get
  lookupLocation $ env Map.! name

newloc :: InterpreterState Location
newloc = do
  (a, (b, new), c, d) <- get
  put (a, (b, new + 1), c, d)
  return new

putInStore :: Object -> InterpreterState Location
putInStore obj = do
  loc <- newloc
  (a, (store, b), c, d) <- get
  let store' = Map.insert loc obj store
  put (a, (store', b), c, d)
  pure loc

putInEnv :: String -> Location -> InterpreterState ()
putInEnv ident loc = do
  (environment, b, c, d) <- get
  let environment' = Map.insert ident loc environment
  put (environment', b, c, d)
  pure ()

defaultObject :: Type -> Object
defaultObject (Type t) = case t of
  "Int" -> IntObject 0
  "String" -> StringObject ""
  "Bool" -> BoolObject False
  _ -> VoidObject

typeOfObject :: Object -> String
typeOfObject obj = case obj of
  Object (Type t) _ -> t
  IntObject _ -> "Int"
  BoolObject _ -> "Bool"
  StringObject _ -> "String"
  VoidObject -> undefined

runMethod :: ClassMap -> ImplementationMap -> ParentMap -> Location -> Method -> InterpreterState Location
runMethod classMap implementationMap parentMap self (Method {methodBody}) = runExpr classMap implementationMap parentMap self methodBody

runExpr :: ClassMap -> ImplementationMap -> ParentMap -> Location -> Typed Expr -> InterpreterState Location
runExpr classMap implementationMap parentMap selfLoc (Typed staticType (Lined lineNumber expr)) = do
  self <- lookupLocation selfLoc
  let runExpr' = runExpr classMap implementationMap parentMap selfLoc
      runExpr'' e = do
        e' <- runExpr' e
        lookupLocation e'
      runExpr''' e = do
        e' <- runExpr' e
        e'' <- lookupLocation e'
        pure (e', e'')
   in case expr of
        DynamicDispatch lhs (Identifier _ methodName) args -> do
          args' <- traverse runExpr' args
          (lhs', lhsV) <- runExpr''' lhs
          case lhsV of
            VoidObject -> runtimeError lineNumber "dispatch on void"
            _ -> pure ()
          let Method {methodBody, methodFormals} = lookupImplementationMap implementationMap (typeOfObject lhsV) methodName
          let args'' = Map.fromList $ zip (map (lexeme . formalName) methodFormals) args'
          (oldEnvironment, b, c, d) <- get
          put (args'', b, c, d)
          result <- runExpr classMap implementationMap parentMap lhs' methodBody
          (_, b', c', d') <- get
          put (oldEnvironment, b', c', d')
          pure result
        StaticDispatch lhs (Identifier _ type') (Identifier _ methodName) args -> do
          let Method {methodBody, methodFormals} = lookupImplementationMap implementationMap type' methodName
          args' <- traverse runExpr' args
          let args'' = Map.fromList $ zip (map (lexeme . formalName) methodFormals) args'
          (lhs', lhsV) <- runExpr''' lhs
          case lhsV of
            VoidObject -> runtimeError lineNumber "static dispatch on void"
            _ -> pure ()
          (oldEnvironment, b, c, d) <- get
          put (args'', b, c, d)
          result <- runExpr classMap implementationMap parentMap lhs' methodBody
          (_, b', c', d') <- get
          put (oldEnvironment, b', c', d')
          pure result
        SelfDispatch (Identifier _ methodName) args -> do
          args' <- traverse runExpr' args
          let Method {methodBody, methodFormals} = lookupImplementationMap implementationMap (typeOfObject self) methodName
          let args'' = Map.fromList $ zip (map (lexeme . formalName) methodFormals) args'
          (oldEnvironment, b, c, d) <- get
          put (args'', b, c, d)
          result <- runExpr classMap implementationMap parentMap selfLoc methodBody
          (_, b', c', d') <- get
          put (oldEnvironment, b', c', d')
          pure result
        If {ifPredicate, trueBody, falseBody} -> do
          predicate <- runExpr' ifPredicate
          predicate' <- lookupLocation predicate
          let BoolObject predicate'' = predicate' -- crash on non-bool predicate
          runExpr' $ if predicate'' then trueBody else falseBody
        While predicate body -> do
          BoolObject predicate' <- runExpr'' predicate
          when predicate' $ do
            _ <- runExpr' body
            _ <- runExpr' $ Typed staticType (Lined lineNumber (While predicate body))
            pure ()
          putInStore VoidObject
        Block body -> do
          body' <- traverse runExpr' body
          pure $ last body'
        Case e elements -> do
          (el, e') <- runExpr''' e
          case e' of
            VoidObject -> runtimeError lineNumber "case on void"
            _ -> pure ()
          let eType = typeOfObject e'
          let branchMap = Map.fromList $ map (\(CaseElement v t body) -> (t, (v, body))) elements
          let matchingBranch t =
                case Map.lookup t branchMap of
                  Just match -> Just match
                  Nothing -> do
                    parent <- Map.lookup t parentMap
                    parentBranch <- matchingBranch parent
                    Just parentBranch
          let branch = matchingBranch $ Type eType
          case branch of
            Just (Identifier _ var, body) -> do
              putInEnv var el
              runExpr' body
            Nothing -> runtimeError lineNumber "case without matching branch"
        New (Identifier _ t) -> do
          let Object selfType _ = self -- self cant be a primitive
          let t0 = if t == "SELF_TYPE" then selfType else Type t
          let attrs = classMap Map.! t0
          attrs' <-
            traverse
              ( \(Attribute (Identifier _ name) attrType expr) -> do
                  loc <- newloc
                  (a, (store, b), c, d) <- get
                  let store' = Map.insert loc (defaultObject attrType) store
                  put (a, (store', b), c, d)
                  pure (loc, name, expr)
              )
              attrs
          objectLoc <- putInStore $ Object t0 (Map.fromList $ map (\(loc, name, _) -> (name, loc)) attrs')
          traverse_
            ( \(location, _, initializer) -> do
                (a, (store, b), c, d) <- get
                store' <- case initializer of
                  Just initializer' -> do
                    v <- runExpr classMap implementationMap parentMap objectLoc initializer'
                    v' <- lookupLocation v
                    pure $ Map.insert location v' store
                  Nothing -> pure store
                put (a, (store', b), c, d)
            )
            attrs'
          pure objectLoc
        Plus a b -> do
          IntObject a' <- runExpr'' a
          IntObject b' <- runExpr'' b
          putInStore $ IntObject $ a' + b'
        Minus a b -> do
          IntObject a' <- runExpr'' a
          IntObject b' <- runExpr'' b
          putInStore $ IntObject $ a' - b'
        Times a b -> do
          IntObject a' <- runExpr'' a
          IntObject b' <- runExpr'' b
          putInStore $ IntObject $ a' * b'
        Divide a b -> do
          IntObject a' <- runExpr'' a
          IntObject b' <- runExpr'' b
          if b' == 0
            then runtimeError lineNumber "division by zero"
            else putInStore $ IntObject $ div a' b'
        LessThan a b -> do
          a' <- runExpr'' a
          b' <- runExpr'' b
          putInStore $ BoolObject $ case a' of
            IntObject a'' -> case b' of
              IntObject b'' -> a'' < b''
              _ -> False
            BoolObject a'' -> case b' of
              BoolObject b'' -> a'' < b''
              _ -> False
            StringObject a'' -> case b' of
              StringObject b'' -> a'' < b''
              _ -> False
            Object _ _ -> False
            VoidObject -> False
        LessThanOrEqualTo a b -> do
          (al, av) <- runExpr''' a
          (bl, bv) <- runExpr''' b
          putInStore $ BoolObject $ case av of
            IntObject a' -> case bv of
              IntObject b' -> a' <= b'
              _ -> False
            BoolObject a' -> case bv of
              BoolObject b' -> a' <= b'
              _ -> False
            StringObject a' -> case bv of
              StringObject b' -> a' <= b'
              _ -> False
            Object _ _ -> al == bl
            VoidObject -> case bv of
              VoidObject -> True
              _ -> False
        Equal a b -> do
          (al, av) <- runExpr''' a
          (bl, bv) <- runExpr''' b
          putInStore $ BoolObject $ case av of
            IntObject a' -> case bv of
              IntObject b' -> a' == b'
              _ -> False
            BoolObject a' -> case bv of
              BoolObject b' -> a' == b'
              _ -> False
            StringObject a' -> case bv of
              StringObject b' -> a' == b'
              _ -> False
            Object _ _ -> al == bl
            VoidObject -> case bv of
              VoidObject -> True
              _ -> False
        IsVoid e -> do
          e' <- runExpr'' e
          putInStore $
            BoolObject
              ( case e' of
                  VoidObject -> True
                  _ -> False
              )
        Not a -> do
          BoolObject a' <- runExpr'' a
          putInStore $ BoolObject $ not a'
        Negate a -> do
          IntObject a' <- runExpr'' a
          putInStore $ IntObject $ -a'
        IntegerConstant i -> putInStore $ IntObject i
        StringConstant s -> putInStore $ StringObject s
        BooleanConstant b -> putInStore $ BoolObject b
        Variable (Identifier _ v) ->
          if v == "self"
            then pure selfLoc
            else do
              (environment, (_, _), _, _) <- get
              let Object _ attrEnv = self
              pure $ fromMaybe (attrEnv Map.! v) $ Map.lookup v environment
        Assign lhs rhs -> do
          rhs' <- runExpr' rhs
          assign self lhs rhs'
          pure rhs'
        Let bindings body -> do
          (oldEnvironment, _, _, _) <- get
          traverse_
            ( \LetBinding {letBindingName = Identifier _ letBindingName, letBindingRhs, letBindingType'} -> do
                rhs <- case letBindingRhs of
                  Just rhs -> runExpr' rhs
                  Nothing -> putInStore $ defaultObject letBindingType'
                putInEnv letBindingName rhs
            )
            bindings
          body' <- runExpr' body
          (_, b, c, d) <- get
          put (oldEnvironment, b, c, d)
          pure body'
        -- COOL internal expressions -> undefined
        InputInternal internal -> case internal of
          IOInInt -> requireInput
          IOInString -> requireInput
          IOOutInt -> do
            IntObject x <- lookupVariable "x"
            (a, b, c, output) <- get
            put (a, b, c, output ++ show x)
            pure selfLoc
          IOOutString -> do
            StringObject x <- lookupVariable "x"
            (a, b, c, output) <- get
            put (a, b, c, output ++ outString x)
            pure selfLoc
          ObjectAbort -> abort
          ObjectCopy -> let Object t attrs = self in putInStore $ Object t attrs
          ObjectTypeName -> let Object (Type t) _ = self in putInStore $ StringObject t
          StringConcat -> do
            StringObject s <- lookupVariable "s"
            let StringObject self' = self
            putInStore $ StringObject $ self' ++ s
          StringLength -> let StringObject self' = self in putInStore $ IntObject $ fromIntegral $ length self'
          StringSubstr -> do
            let StringObject self' = self
            IntObject i <- lookupVariable "i"
            IntObject l <- lookupVariable "l"
            let i' = fromIntegral i
            let l' = fromIntegral l
            if i' < 0 || i' >= length self' || i' + l' > length self' || l' < 0
              then runtimeError lineNumber "String.substr out of range"
              else putInStore $ StringObject $ drop i' (take (i' + l') self')
        Constructor -> undefined -- wont happen

outString :: String -> String
outString = outString' False

outString' :: Bool -> String -> String
outString' backslash (c : remaining) =
  if backslash
    then
      ( case c of
          '\\' -> "\\"
          'n' -> "\n"
          't' -> "\t"
          _ -> "\\" ++ [c]
      )
        ++ outString' False remaining
    else case c of
      '\\' -> outString' True remaining
      _ -> c : outString' False remaining
outString' backslash [] = if backslash then "\\" else ""
