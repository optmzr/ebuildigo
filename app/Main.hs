module Main where

import Lib

main :: IO [()]
main = do
  contents <- getContents
  let modules = parseModules contents
  traverse print modules
