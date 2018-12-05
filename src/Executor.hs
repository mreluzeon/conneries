module Executor where

import Control.Monad.State
import Data.Ratio

import Types

instance Num LispValue where
  (Ratio x)   + (Ratio y) = Ratio (x + y)
  (Ratio x)   + y@(List  _) = (Ratio x) + (executor y)
  x@(List  _) + (Ratio y) = (executor x) + (Ratio y)
  x@(List _) + y@(List _) = (executor x) + (executor y)
  (Ratio x)   - (Ratio y) = Ratio (x - y)
  (Ratio x)   - y@(List  _) = (Ratio x) - (executor y)
  x@(List  _) - (Ratio y) = (executor x) - (Ratio y)
  x@(List _) - y@(List _) = (executor x) - (executor y)
  (Ratio x)   * (Ratio y) = Ratio (x * y)
  (Ratio x)   * y@(List  _) = (Ratio x) * (executor y)
  x@(List _) * (Ratio y) = (executor x) * (Ratio y)
  x@(List _) * y@(List _) = (executor x) * (executor y)
  negate (Ratio x) = Ratio $ negate x
  negate x@(List _) = negate $ executor x
  abs (Ratio x) = Ratio $ abs x
  abs x@(List _) = abs $ executor x
  signum (Ratio x) = Ratio $ signum x
  signum x@(List _) = signum $ executor x
  fromInteger x = Ratio $ fromInteger x

instance Fractional LispValue where
  (Ratio x) / (Ratio y) = Ratio (x / y)
  (Ratio x) / y@(List _) = (Ratio x) / (executor y)
  x@(List _) / (Ratio y) = (executor x) / (Ratio y)
  x@(List _) / y@(List _) = (executor x) / (executor y)
  recip (Ratio x) = Ratio $ recip x
  recip x@(List _) = recip $ executor x
  fromRational x = Ratio $ fromRational x

-- uUpdate k v hash = if M.member k hash
--                    then M.update (\_ -> v) k hash
--                    else M.insert k v hash

-- executor (List (x:xs)) = Function (\[x] -> x)

fromrat :: LispValue -> Float
fromrat (Ratio x) = fromRational x
fromrat x@(List _)  = fromrat $ executor x

executor (List (x:xs)) = execFunction x xs
executor a@(Ratio _) = a
executor a@(String _) = a
executor a@(Bool _) = a
executor a@(Word _) = a
executor a@(Keyword _) = a

execFunction (Word "+") (x:xs) = foldl (\a c -> c + a) x xs
execFunction (Word "-") (x:xs) = foldl (\a c -> a - c) x xs
execFunction (Word "*") (x:xs) = foldl (\a c -> c * a) x xs
execFunction (Word "/") (x:xs) = foldl (\a c -> a / c) x xs
execFunction (Word "float") [x] = String $ show $ fromrat x
execFunction (Word "abs") [x] = abs x

exec (Left a)  = Log $ show a
exec (Right a) = executor a

-- fromLispRatio (Number x) = toRational x
-- fromLispRatio (Ratio  x) = x
