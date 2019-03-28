module Main where

import Lib
import System.IO

main :: IO [()]
main = do
  contents <- getContents
  let modules = parseGoModules contents
  deps <- mapM toEgoDep modules
  mapM printEgoDep deps
