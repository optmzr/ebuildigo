module Main where

import Lib

main :: IO [()]
main = do
  contents <- getContents
  let modules = parseGoModules contents
  let deps = map toEgoDep modules
  traverse print deps
