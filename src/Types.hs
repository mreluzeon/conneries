module Types where

import Data.Ratio

data LispValue = Ratio Rational
               | Bool Bool
               | String String
               | Keyword String
               | Word String
               | List [LispValue]
               | Quoted [LispValue]
               | Log String
               | Printer (IO ())
               | Function ([LispValue] -> LispValue)
               -- | BuiltInUnary (LispValue) -> LispValue
               -- | BuiltInBinary (LispValue, LispValue) -> LispValue
               -- | BuiltInTernary (LispValue, LispValue, LispValue) -> LispValue

instance Show LispValue where
  show (Ratio     a) = (((++) "Ratio ") . show) a
  show (Bool      a) = (((++) "Bool ") . show) a
  show (String    a) = (((++) "String ") . show) a  
  show (Keyword   a) = (((++) "Keyword ") . show) a
  show (Word      a) = (((++) "Word ") . show) a
  show (List      a) = (((++) "List ") . show) a
  show (Quoted    a) = (((++) "Quoted ") . show) a
  show (Log       a) = (((++) "Log ") . show) a
  show (Printer   a) = "Printer"
  show (Function  _) = "Function"
