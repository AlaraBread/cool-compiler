{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Trac where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Distribution.Simple.Utils (lowercase)
import InputIr (Method (methodFormals))
import qualified InputIr
import Util

data TracIr v = TracIr
  { implementationMap :: Map.Map Type [InputIr.ImplementationMapEntry (TracMethod v)],
    typeDetailsMap :: TypeDetailsMap
  }
  deriving (Show)

type TypeDetailsMap = Map.Map Type TypeDetails

-- Type size is in words.
data TypeDetails = TypeDetails {typeTag :: Int, typeSize :: Int, methodTags :: Map.Map String Int}
  deriving (Show)

data TracMethod v = TracMethod {methodName :: String, body :: Trac v, formals :: [InputIr.Formal], temporaryCount :: Int}
  deriving (Show)

type Trac v = [Lined (TracStatement v)]

data TracStatement v
  = Add v v v
  | Subtract v v v
  | Multiply v v v
  | Divide v v v
  | LessThan v v v
  | LessThanOrEqualTo v v v
  | Equals v v v
  | IntConstant v Int
  | BoolConstant v Bool
  | StringConstant v String
  | Not v v
  | Negate v v
  | New v Type
  | Default v Type
  | IsVoid v v
  | Dispatch
      { dispatchResult :: v,
        dispatchReceiver :: v,
        dispatchReceiverType :: Type,
        dispatchType :: Maybe Type,
        dispatchMethod :: String,
        dispatchArgs :: [v]
      }
  | Jump Label
  | TracLabel Label
  | Return v
  | Comment String
  | ConditionalJump v Label
  | Assign v v
  | -- The map *must* cover every possible type, as later this gets lowered to a jump table
    -- dst, src, jumptable
    Case v v (Map.Map Type Label)
  | TracInternal InputIr.Internal
  | Abort Int (AbortReason v)

data AbortReason v
  = DispatchOnVoid
  | StaticDispatchOnVoid
  | CaseOnVoid
  | CaseNoMatch v
  | DivisionByZero
  | SubstringOutOfRange
  deriving (Show)

instance (Show v) => Show (TracStatement v) where
  show statement = case statement of
    Add a b c -> showBinary a b c "+"
    Subtract a b c -> showBinary a b c "-"
    Multiply a b c -> showBinary a b c "*"
    Divide a b c -> showBinary a b c "/"
    LessThan a b c -> showBinary a b c "<"
    LessThanOrEqualTo a b c -> showBinary a b c "<="
    Equals a b c -> showBinary a b c "="
    IntConstant v i -> show v ++ " <- int " ++ show i
    BoolConstant v b -> show v ++ " <- bool " ++ lowercase (show b)
    StringConstant v s -> show v ++ " <- string\n" ++ s
    Not a b -> showUnary a b "not"
    Negate a b -> showUnary a b "~"
    New a (Type t) -> show a ++ " <- new " ++ t
    Default a (Type t) -> show a ++ " <- default " ++ t
    IsVoid a b -> showUnary a b "isvoid"
    Dispatch
      { dispatchResult,
        dispatchMethod,
        dispatchArgs
      } -> show dispatchResult ++ " <- call " ++ dispatchMethod ++ " " ++ unwords (map show dispatchArgs)
    Jump l -> "jmp " ++ show l
    TracLabel l -> "label " ++ show l
    Return a -> "return " ++ show a
    Comment msg -> "comment " ++ msg
    ConditionalJump a l -> "bt " ++ show a ++ " " ++ show l
    Assign a b -> show a ++ " <- " ++ show b
    Case dst src labels -> show dst ++ " <- case " ++ show src ++ " " ++ show labels
    TracInternal internal -> "internal: " ++ show internal
    Abort line reason -> "abort " ++ show line ++ " " ++ show reason

showBinary :: (Show v) => v -> v -> v -> String -> String
showBinary a b c op = show a ++ " <- " ++ op ++ " " ++ show b ++ " " ++ show c

showUnary :: (Show v) => v -> v -> String -> String
showUnary a b op = show a ++ " <- " ++ op ++ " " ++ show b

generateTracExpr ::
  ([Type] -> Map.Map Type (Maybe Type)) ->
  InputIr.ClassMap ->
  Map.Map String Variable ->
  Type ->
  InputIr.Typed InputIr.Expr ->
  State Temporary (Trac Variable, Variable)
generateTracExpr
  pickLowestParents
  classMap
  bindingMap
  selfType
  InputIr.Typed
    { InputIr.type',
      InputIr.item = Lined lineNumber expr
    } =
    let lined :: InputIr.Typed InputIr.Expr -> a -> Lined a
        lined e = Lined (line $ InputIr.item e)

        lined' :: a -> Lined a
        lined' = Lined lineNumber

        compose2 :: (b -> c) -> (a -> a -> b) -> (a -> a -> c)
        compose2 g f p1 p2 = g $ f p1 p2

        compose3 :: (b -> c) -> (a -> a -> a -> b) -> (a -> a -> a -> c)
        compose3 g f p1 p2 p3 = g $ f p1 p2 p3

        linedUnary = compose2 lined'
        linedBinary = compose3 lined'

        generateTracExpr' = generateTracExpr pickLowestParents classMap bindingMap selfType
     in case expr of
          InputIr.Assign
            InputIr.Identifier
              { InputIr.lexeme
              }
            exp ->
              do
                (trac, variable) <- generateTracExpr' exp
                let lhs = resolveVariable bindingMap lexeme
                return (trac ++ [lined' $ Assign lhs variable], variable)
          InputIr.DynamicDispatch
            { InputIr.dynamicDispatchLhs = receiver,
              InputIr.dynamicDispatchMethod = method,
              InputIr.dynamicDispachArgs = args
            } -> do
              temp <- getVariable
              dispatchArgs' <- mapM generateTracExpr' args
              let dispatchArgsTrac = concatMap fst dispatchArgs'
              let dispatchArgsV = map snd dispatchArgs'
              (receiverTrac, receiverV) <- generateTracExpr' receiver

              isVoid <- getVariable
              isNotVoid <- getVariable
              isNotVoidLabel <- getLabel
              pure
                ( dispatchArgsTrac
                    ++ receiverTrac
                    ++ [ lined' $ IsVoid isVoid receiverV,
                         lined' $ Not isNotVoid isVoid,
                         lined' $ ConditionalJump isNotVoid isNotVoidLabel,
                         lined' $ Abort lineNumber DispatchOnVoid,
                         lined' $ TracLabel isNotVoidLabel,
                         lined' $
                           Dispatch
                             { dispatchResult = temp,
                               dispatchMethod = InputIr.lexeme method,
                               dispatchReceiver = receiverV,
                               dispatchReceiverType = InputIr.type' receiver,
                               dispatchType = Nothing,
                               dispatchArgs = dispatchArgsV
                             }
                       ],
                  temp
                )
          InputIr.StaticDispatch
            { InputIr.staticDispatchLhs = receiver,
              InputIr.dynamicDispatchType = type',
              InputIr.dynamicDispatchMethod = method,
              InputIr.dynamicDispatchArgs = args
            } -> do
              temp <- getVariable
              dispatchArgs' <- mapM generateTracExpr' args
              let dispatchArgsTrac = concatMap fst dispatchArgs'
              let dispatchArgsV = map snd dispatchArgs'
              (receiverTrac, receiverV) <- generateTracExpr' receiver

              isVoid <- getVariable
              isNotVoid <- getVariable
              isNotVoidLabel <- getLabel
              pure
                ( dispatchArgsTrac
                    ++ receiverTrac
                    ++ [ lined' $ IsVoid isVoid receiverV,
                         lined' $ Not isNotVoid isVoid,
                         lined' $ ConditionalJump isNotVoid isNotVoidLabel,
                         lined' $ Abort lineNumber StaticDispatchOnVoid,
                         lined' $ TracLabel isNotVoidLabel,
                         lined' $
                           Dispatch
                             { dispatchResult = temp,
                               dispatchMethod = InputIr.lexeme method,
                               dispatchReceiver = receiverV,
                               dispatchReceiverType = InputIr.type' receiver,
                               dispatchType = Just $ Type $ InputIr.lexeme type',
                               dispatchArgs = dispatchArgsV
                             }
                       ],
                  temp
                )
          InputIr.SelfDispatch
            { InputIr.selfDispatchMethod,
              InputIr.selfDispatchArgs
            } -> do
              temp <- getVariable
              dispatchArgs' <- mapM generateTracExpr' selfDispatchArgs
              let dispatchArgsTrac = concatMap fst dispatchArgs'
              let dispatchArgsV = map snd dispatchArgs'
              pure
                ( dispatchArgsTrac
                    ++ [ lined' $
                           Dispatch
                             { dispatchResult = temp,
                               dispatchMethod = InputIr.lexeme selfDispatchMethod,
                               dispatchReceiver = ParameterV 0,
                               dispatchReceiverType = selfType,
                               dispatchType = Nothing,
                               dispatchArgs = dispatchArgsV
                             }
                       ],
                  temp
                )
          InputIr.If
            { InputIr.ifPredicate,
              InputIr.trueBody,
              InputIr.falseBody
            } -> do
              (predicateTrac, predicateV) <- generateTracExpr' ifPredicate
              (trueTrac, trueV) <- generateTracExpr' trueBody
              (falseTrac, falseV) <- generateTracExpr' falseBody
              trueLabel <- getLabel
              trueEndLabel <- getLabel
              bodyV <- getVariable
              let trueTrac' =
                    lined trueBody (TracLabel trueLabel)
                      : trueTrac
                      ++ [ lined trueBody (Assign bodyV trueV),
                           lined trueBody (TracLabel trueEndLabel)
                         ]
              let falseTrac' =
                    falseTrac
                      ++ [ lined falseBody (Assign bodyV falseV),
                           lined falseBody $ Jump trueEndLabel
                         ]
              return
                ( predicateTrac
                    ++ [lined ifPredicate $ ConditionalJump predicateV trueLabel]
                    ++ falseTrac'
                    ++ trueTrac',
                  bodyV
                )
          InputIr.While
            { InputIr.whilePredicate,
              InputIr.whileBody
            } -> do
              (predicateTrac, predicateV) <- generateTracExpr' whilePredicate
              (bodyTrac, _) <- generateTracExpr' whileBody
              outV <- getVariable
              whileStart <- getLabel
              endLabel <- getLabel
              notPredicateV <- getVariable
              pure
                ( [lined whileBody $ TracLabel whileStart]
                    ++ predicateTrac
                    ++ [lined whilePredicate $ Not notPredicateV predicateV]
                    ++ [lined whilePredicate $ ConditionalJump notPredicateV endLabel]
                    ++ bodyTrac
                    ++ [lined whilePredicate $ Jump whileStart]
                    ++ [lined whilePredicate $ TracLabel endLabel]
                    ++ [lined whilePredicate $ Default outV $ Type "Object"],
                  outV
                )
          InputIr.Block expressions -> do
            expressions <- traverse generateTracExpr' expressions
            pure
              ( concatMap fst expressions,
                snd $ last expressions -- this will crash on an empty block
              )
          InputIr.New InputIr.Identifier {InputIr.lexeme = typeName} -> do
            t <- getVariable
            pure ([lined' $ New t $ Type typeName], t)
          InputIr.IsVoid exp -> unaryOperation generateTracExpr' exp $ linedUnary IsVoid
          InputIr.Plus a b -> binaryOperation generateTracExpr' a b $ linedBinary Add
          InputIr.Minus a b -> binaryOperation generateTracExpr' a b $ linedBinary Subtract
          InputIr.Times a b -> binaryOperation generateTracExpr' a b $ linedBinary Multiply
          InputIr.Divide a b -> do
            (aTrac, aV) <- generateTracExpr' a
            (bTrac, bV) <- generateTracExpr' b

            isNotZeroLabel <- getLabel
            zero <- getVariable
            isZero <- getVariable
            isNotZero <- getVariable

            resultV <- getVariable
            pure
              ( aTrac
                  ++ bTrac
                  ++ [ lined' $ IntConstant zero 0,
                       lined' $ Equals isZero zero bV,
                       lined' $ Not isNotZero isZero,
                       lined' $ ConditionalJump isNotZero isNotZeroLabel,
                       lined' $ Abort lineNumber DivisionByZero,
                       lined' $ TracLabel isNotZeroLabel,
                       linedBinary Divide resultV aV bV
                     ],
                resultV
              )
          InputIr.LessThan a b -> binaryOperation generateTracExpr' a b $ linedBinary LessThan
          InputIr.LessThanOrEqualTo a b -> binaryOperation generateTracExpr' a b $ linedBinary LessThanOrEqualTo
          InputIr.Equal a b -> binaryOperation generateTracExpr' a b $ linedBinary Equals
          InputIr.Not exp -> unaryOperation generateTracExpr' exp $ linedUnary Not
          InputIr.Negate exp -> unaryOperation generateTracExpr' exp $ linedUnary Negate
          InputIr.IntegerConstant i -> constant lineNumber IntConstant i
          InputIr.StringConstant s -> constant lineNumber StringConstant s
          InputIr.BooleanConstant b -> constant lineNumber BoolConstant b
          InputIr.Variable InputIr.Identifier {InputIr.lexeme} ->
            pure ([], resolveVariable bindingMap lexeme)
          -- Handle Let with no bindings; this is our base case.
          InputIr.Let [] body -> generateTracExpr' body
          InputIr.Let ((InputIr.LetBinding bindingName bindingType rhs) : rest) body -> do
            bindingV <- getVariable
            let bindingMap' = Map.insert (InputIr.lexeme bindingName) bindingV bindingMap

            -- Even with an right hand side, we need to default initialize the
            -- object so that it is accessible in the initializer.

            let defaultInitializer =
                  [ lined' $ Comment $ InputIr.lexeme bindingName ++ " <- default",
                    lined' $ Default bindingV bindingType
                  ]

            initializer <- case rhs of
              Just rhs' -> do
                (rhsTrac, rhsV) <- generateTracExpr pickLowestParents classMap bindingMap selfType rhs'
                pure $
                  rhsTrac
                    ++ [ Lined (InputIr.line bindingName) $ Comment $ InputIr.lexeme bindingName ++ " <- rhs",
                         Lined (InputIr.line bindingName) $ Assign bindingV rhsV
                       ]
              Nothing -> pure []

            (restTrac, restV) <-
              generateTracExpr pickLowestParents classMap bindingMap' selfType $
                InputIr.Typed type' $
                  lined' $
                    InputIr.Let rest body

            pure (defaultInitializer ++ initializer ++ restTrac, restV)
          InputIr.Case e elements -> do
            (caseVariableTrac, caseVariable) <- generateTracExpr' e
            resultVariable <- getVariable
            endLabel <- getLabel
            elements' <-
              traverse
                ( \InputIr.CaseElement
                     { InputIr.caseElementVariable = InputIr.Identifier {InputIr.lexeme = caseElementVariable},
                       InputIr.caseElementType,
                       InputIr.caseElementBody
                     } -> do
                      let bindingMap' = Map.insert caseElementVariable caseVariable bindingMap
                      label <- getLabel
                      (body, bodyVariable) <-
                        generateTracExpr pickLowestParents classMap bindingMap' selfType caseElementBody
                      pure $
                        ( caseElementType,
                          label,
                          [lined' $ TracLabel label] ++ body ++ [lined' $ Assign resultVariable bodyVariable, lined' $ Jump endLabel]
                        )
                )
                elements

            let caseTypeToLabelMap = Map.fromList $ map (\(type', label, _) -> (type', label)) elements'
            let trac = concatMap (\(_, _, t) -> t) elements'

            abortLabel <- getLabel
            typeName <- getVariable
            let abortCode =
                  map
                    lined'
                    [ TracLabel abortLabel,
                      Dispatch
                        { dispatchReceiver = caseVariable,
                          dispatchArgs = [],
                          dispatchMethod = "type_name",
                          dispatchType = Nothing,
                          dispatchReceiverType = Type "Object",
                          dispatchResult = typeName
                        },
                      Abort lineNumber $ CaseNoMatch typeName
                    ]

            let typeToParentMap = pickLowestParents (Map.keys caseTypeToLabelMap)
            let typeToLabel (Just p) = caseTypeToLabelMap Map.! p
                typeToLabel Nothing = abortLabel
            let typeToLabelMap = Map.map typeToLabel typeToParentMap

            isVoid <- getVariable
            isNotVoid <- getVariable
            isNotVoidLabel <- getLabel
            pure
              ( caseVariableTrac
                  ++ [ lined' $ IsVoid isVoid caseVariable,
                       lined' $ Not isNotVoid isVoid,
                       lined' $ ConditionalJump isNotVoid isNotVoidLabel,
                       lined' $ Abort lineNumber CaseOnVoid,
                       lined' $ TracLabel isNotVoidLabel,
                       lined' $ Case resultVariable caseVariable typeToLabelMap
                     ]
                  ++ trac
                  ++ abortCode
                  ++ [lined' $ TracLabel endLabel],
                resultVariable
              )
          InputIr.InputInternal internal -> ([Lined 0 $ TracInternal internal],) <$> getVariable
          InputIr.Constructor -> do
            constructor <- generateTracConstructor pickLowestParents classMap selfType
            eV <- getVariable
            pure (constructor, eV)

binaryOperation ::
  ( InputIr.Typed InputIr.Expr ->
    State Temporary (Trac Variable, Variable)
  ) ->
  InputIr.Typed InputIr.Expr ->
  InputIr.Typed InputIr.Expr ->
  (Variable -> Variable -> Variable -> Lined (TracStatement Variable)) ->
  State Temporary (Trac Variable, Variable)
binaryOperation generateTracExpr' a b op = do
  (aTrac, aV) <- generateTracExpr' a
  (bTrac, bV) <- generateTracExpr' b
  resultV <- getVariable
  pure (aTrac ++ bTrac ++ [op resultV aV bV], resultV)

unaryOperation ::
  ( InputIr.Typed InputIr.Expr ->
    State Temporary (Trac Variable, Variable)
  ) ->
  InputIr.Typed InputIr.Expr ->
  (Variable -> Variable -> Lined (TracStatement Variable)) ->
  State Temporary (Trac Variable, Variable)
unaryOperation generateTracExpr' exp op = do
  (trac, v) <- generateTracExpr' exp
  resultV <- getVariable
  pure (trac ++ [op resultV v], resultV)

constant :: Int -> (Variable -> a -> TracStatement Variable) -> a -> State Temporary (Trac Variable, Variable)
constant line statement c = do
  resultV <- getVariable
  pure ([Lined line $ statement resultV c], resultV)

-- If we do not have a match, that means we did something wrong, as we are
-- guaranteed to have correct bindings by the semantic analyzer.
resolveVariable :: Map.Map String Variable -> String -> Variable
resolveVariable bindingMap variable = bindingMap Map.! variable

generateAttributeMap :: [InputIr.Attribute] -> Map.Map String Variable
generateAttributeMap attributes =
  Map.fromList $
    zip
      (map (InputIr.lexeme . InputIr.attrName) attributes)
      (map AttributeV [0 ..])

generateTracMethod ::
  ([Type] -> Map.Map Type (Maybe Type)) ->
  InputIr.ClassMap ->
  [InputIr.Attribute] ->
  Type ->
  InputIr.Method ->
  State Temporary (TracMethod Variable)
generateTracMethod pickLowestParents classMap attributes type' (InputIr.Method {InputIr.methodName, InputIr.methodFormals, InputIr.methodBody}) = do
  modify (\(Temporary label _) -> Temporary label 0)

  let temporaryMap = Map.empty

  let paramNames = "self" : map (InputIr.lexeme . InputIr.formalName) methodFormals
  let paramMap = Map.fromList $ zip paramNames (map ParameterV [0 ..])

  let attributeMap = generateAttributeMap attributes

  let bindingMap = Map.union paramMap attributeMap

  (trac, v) <- generateTracExpr pickLowestParents classMap bindingMap type' methodBody
  temporaryCount' <- gets (\(Temporary _ temporary) -> temporary)
  let Type typeName = type'
      -- Do not touch internal instructions
      body = case trac of
        [Lined _ (TracInternal _)] -> trac
        t -> t ++ [Lined (InputIr.line methodName) $ Return v]
   in pure
        TracMethod
          { methodName = InputIr.lexeme methodName,
            body,
            formals = methodFormals,
            temporaryCount = temporaryCount'
          }

generateTracConstructor ::
  ([Type] -> Map.Map Type (Maybe Type)) ->
  InputIr.ClassMap ->
  Type ->
  State Temporary (Trac Variable)
generateTracConstructor pickLowestParents classMap selfType = do
  let attrs = classMap Map.! selfType
  let attributeMap = generateAttributeMap attrs
  let bindingMap = Map.insert "self" (ParameterV 0) attributeMap
  let attrs' = zip attrs [0 ..]

  let defaultInitialize =
        concatMap
          ( \( InputIr.Attribute
                 { InputIr.attrName = InputIr.Identifier {InputIr.line},
                   InputIr.attrType
                 },
               idx
               ) -> [Lined line $ Default (AttributeV idx) attrType]
          )
          attrs'

  initialize <-
    concat
      <$> traverse
        ( \( InputIr.Attribute
               { InputIr.attrName = InputIr.Identifier {InputIr.line},
                 InputIr.attrRhs
               },
             idx
             ) -> case attrRhs of
              Just e -> do
                (trac, v) <- generateTracExpr pickLowestParents classMap bindingMap selfType e
                pure $ trac ++ [Lined line $ Assign (AttributeV idx) v]
              Nothing -> pure []
        )
        attrs'

  pure $ defaultInitialize ++ initialize

generateTrac ::
  ([Type] -> Map.Map Type (Maybe Type)) ->
  InputIr.InputIr ->
  (TracIr Variable, Temporary)
generateTrac pickLowestParents (InputIr.InputIr classMap implMap _ _) =
  runState
    ( do
        implMap' <-
          sequence $
            Map.mapWithKey
              ( mapM
                  . ( \name method -> traverse (generateTracMethod pickLowestParents classMap (classMap Map.! name) name) method
                    )
              )
              implMap
        pure
          TracIr
            { implementationMap = implMap',
              typeDetailsMap =
                snd $
                  Map.mapAccumWithKey
                    ( \tag type' attrs ->
                        ( tag + 1,
                          TypeDetails
                            tag
                            ( case type' of
                                Type "Int" -> 4
                                Type "Bool" -> 4
                                -- string itself, length, capacity
                                Type "String" -> 6
                                Type _ -> 3 + length attrs
                            )
                            $ Map.fromList
                            $ zip
                              (map (InputIr.implementationMapEntryName ((\(InputIr.Identifier _ s) -> s) . InputIr.methodName)) $ implMap Map.! type')
                              [0 ..]
                        )
                    )
                    0
                    classMap
            }
    )
    ( Temporary
        0
        0
    )
