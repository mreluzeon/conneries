module Executor where

import Control.Monad.State
import Data.Ratio

import Types

-- uUpdate k v hash = if M.member k hash
--                    then M.update (\_ -> v) k hash
--                    else M.insert k v hash

-- executor (List (x:xs)) = Function (\[x] -> x)
executor (List (x:xs)) = execFunction x xs
executor a@(Number _) = a
executor a@(Ratio _) = a
executor a@(String _) = a
executor a@(Bool _) = a
executor a@(Word _) = a
executor a@(Keyword _) = a

execFunction (Word "+") (x:xs) = Number $ foldl (\a c -> fromLispNumber c + a) (fromLispNumber x) xs
execFunction (Word "-") (x:xs) = Number $ foldl (\a c -> a - (fromLispNumber c)) (fromLispNumber x) xs

fromLispNumber :: LispValue -> Float
fromLispNumber (Number x) = x
fromLispNumber (Ratio x) = fromRational $ toRational x

exec (Left a)  = Log $ show a
exec (Right a) = executor a

-- fromLispRatio (Number x) = toRational x
-- fromLispRatio (Ratio  x) = x
