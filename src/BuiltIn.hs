module BuiltIn where

import Data.Ratio

import Types

fromLispNumber (Number x) = x
fromLispRatio  (Ratio  x) = x

plus :: [LispValue] -> LispValue
plus = Number . foldl (\a c -> fromLispNumber c + a) 0

rplus :: [LispValue] -> LispValue
rplus = Ratio . foldl (\a c -> fromLispRatio c + a) 0
