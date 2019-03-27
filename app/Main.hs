module Main where

import Lib
import System.IO

main :: IO ()
main = do
  contents <- getContents
  let modules = parseGoModules contents
  let deps = map toEgoDep modules
  traverse print deps
  dirs <- getDirectories "/home/willeponken/go/pkg/mod/cache/vcs/"
  (_, stdout, stderr) <-
    getLongHash
      ("/home/willeponken/go/pkg/mod/cache/vcs/" ++ (head dirs))
      "abcdef"
  print dirs
  print stdout
  print stderr
