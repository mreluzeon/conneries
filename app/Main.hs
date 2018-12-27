module Main where

import Lib
import Executor
import Parser

import Text.Parsec
import System.IO  
import Control.Monad

main :: IO ()
main = do
  a <- getLine
  putStrLn $ show $ exec $ parse parseList "" a
  return ()