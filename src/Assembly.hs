import Control.Monad.State
import Data.Map (mapWithKey)
import qualified Data.Map.Strict as Map
import qualified InputIr
import Trac (Label (..), Temporary (Temporary), Variable (AttributeV, ParameterV, TemporaryV), getVariable)
import qualified Twac
import qualified TwacR

data AssemblyStatement
  = Add TwacR.Register TwacR.Register
  | Subtract TwacR.Register TwacR.Register
  | Multiply TwacR.Register TwacR.Register
  | Divide TwacR.Register
  | Cqto
  | Cmp TwacR.Register TwacR.Register
  | Test TwacR.Register TwacR.Register
  | TestConst TwacR.Register Integer
  | Store TwacR.Register Address
  | Load Address TwacR.Register
  | LoadConst Integer TwacR.Register
  | Transfer TwacR.Register TwacR.Register
  | Push TwacR.Register
  | Pop TwacR.Register
  | Not TwacR.Register
  | Negate TwacR.Register
  | AssemblyLabel Label
  | Call Label
  | DynamicCall TwacR.Register
  | Return
  | Jump Label
  | JumpZero Label
  | JumpNonZero Label
  | JumpLessThan Label
  | JumpLessThanEqual Label
  | LoadLabel Label TwacR.Register
  | JumpIndirect TwacR.Register
  | Comment String

data AssemblyData
  = JumpTable Label [Label]
  | StringConstant Label String
  | VTable
      { vTableTypeName :: Label,
        vTableTypeId :: Integer,
        vTableMethods :: [Label]
      }

data Address = Address
  { addressOffset :: Maybe Integer,
    addressBase :: Maybe TwacR.Register,
    addressIndex :: Maybe TwacR.Register,
    addressScale :: Maybe Integer
  }

data AssemblyIr = AssemblyIr
  { assemblyIrCode :: [AssemblyStatement],
    assemblyIrData :: [AssemblyData]
  }

generateAssembly :: TwacR.TwacRIr -> AssemblyIr
generateAssembly TwacR.TwacRIr {TwacR.implementationMap, TwacR.constructorMap} =
  AssemblyIr
    { assemblyIrCode = concat $ Map.mapWithKey (concatMap . generateAssemblyMethod) implementationMap,
      assemblyIrData = undefined
    }

generateAssemblyMethod :: InputIr.Type -> TwacR.TwacRMethod -> [AssemblyStatement]
generateAssemblyMethod = undefined

temporaryToAddress :: Temporary -> Address
temporaryToAddress (Temporary t) =
  Address
    { addressOffset = Just $ 8 * toInteger t,
      addressBase = Just TwacR.Rbp,
      addressIndex = Nothing,
      addressScale = Nothing
    }

parameterToRegister :: Int -> TwacR.Register
parameterToRegister p = [TwacR.Rdi, TwacR.Rsi, TwacR.Rdx, TwacR.Rcx, TwacR.R8, TwacR.R9] !! p

attributeAddress :: TwacR.Register -> Int -> Address
attributeAddress objectRegister a =
  Address
    { addressOffset = Just $ 8 * toInteger a,
      addressBase = Just objectRegister,
      addressIndex = Nothing,
      addressScale = Nothing
    }

-- this relies on rdi always having a pointer to self
selfAttributeAddress :: Int -> Address
selfAttributeAddress = attributeAddress TwacR.Rdi

generateAssemblyStatements :: TwacR.TwacRStatement -> [AssemblyStatement]
generateAssemblyStatements twacRStatement =
  case twacRStatement of
    TwacR.Load variable register ->
      case variable of
        TemporaryV t -> [Load (temporaryToAddress t) register]
        ParameterV p -> [Transfer (parameterToRegister p) register]
        AttributeV a -> [Load (selfAttributeAddress a) register]
    TwacR.Store register variable ->
      case variable of
        TemporaryV t -> [Store register (temporaryToAddress t)]
        ParameterV p -> [Transfer register (parameterToRegister p)]
        AttributeV a -> [Store register (selfAttributeAddress a)]
    TwacR.TwacRStatement (Twac.New t register) -> []
    TwacR.TwacRStatement
      Twac.Dispatch
        { Twac.dispatchResult,
          Twac.dispatchMethod,
          Twac.dispatchArgs,
          Twac.dispatchReceiver,
          Twac.dispatchReceiverType,
          Twac.dispatchType
        } ->
        case dispatchType of
          Just (InputIr.Type t) ->
            -- static dispatch
            [ Call $ Label $ t ++ "@" ++ dispatchMethod,
              Transfer TwacR.Rax dispatchResult
            ]
          Nothing ->
            -- dynamic dispatch
            []
