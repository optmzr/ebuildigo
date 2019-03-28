module Main where

import Lib
import System.IO

main :: IO ()
main = do
  contents <- getContents
  let modules = parseGoModules contents
  let deps = map toEgoDep modules
  dirs <- getAbsDirectories "/home/willeponken/go/pkg/mod/cache/vcs/"
  hash <- findLongHash dirs "a34e9553db1e"
  print dirs
  print hash
