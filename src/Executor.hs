module Executor where

import qualified Data.Map as M
import Control.Monad.State

import Types

initialState = M.fromList [("+", Function plus),
                           ("r+", Function rplus)]

uUpdate k v hash = if M.member k hash
                   then M.update (\a-> Just v) k hash
                   else M.insert k v hash

executor :: LispValue -> State (M.Map String LispValue) (M.Map String LispValue)
executor (List (x:xs)) = do
  env <- get
  func <- env `lookup` x
  res <- execute func args
  put $ updateState $ res
  executor 
executor (List []) = return $ get

if 

executeFunc (Function func) args = func args

fromLispNumber (Number x) = x
fromLispRatio  (Ratio  x) = x

plus :: [LispValue] -> LispValue
plus = Number . foldl (\a c -> fromLispNumber c + a) 0

rplus :: [LispValue] -> LispValue
rplus = Ratio . foldl (\a c -> fromLispRatio c + a) 0
