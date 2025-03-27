import Trac (Label)
import TwacR (Register)

data AssemblyStatement
  = Add Register Register
  | Subtract Register Register
  | Multiply Register Register
  | Divide Register
  | Cqto
  | Cmp Register Register
  | Test Register Register
  | TestConst Register Integer
  | Store Register Address
  | Load Address Register
  | LoadConst Integer Register
  | Transfer Register Register
  | Not Register
  | Negate Register
  | AssemblyLabel Label
  | Call Label
  | Return
  | Jump Label
  | JumpZero Label
  | JumpNonZero Label
  | JumpLessThan Label
  | JumpLessThanEqual Label
  | LoadLabel Label Register
  | JumpIndirect Register
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
    addressBase :: Maybe Register,
    addressIndex :: Maybe Register,
    addressScale :: Maybe Integer
  }

data AssemblyIr = AssemblyIr
  { assemblyIrCode :: [AssemblyStatement],
    assemblyIrData :: [AssemblyData]
  }
