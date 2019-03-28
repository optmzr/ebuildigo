-- My try at learning Haskell with some stdin parsing program.
module Lib where

import Control.Monad (filterM)
import Data.List (isSuffixOf)
import GHC.IO.Exception (ExitCode(ExitSuccess))
import System.Directory (canonicalizePath, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.Process
  ( StdStream(CreatePipe)
  , cwd
  , readCreateProcessWithExitCode
  , shell
  , std_out
  )

data GoModule = GoModule
  { path :: String
  , version :: String
  , goMod :: String
  } deriving (Show) -- GoModule deriving _from_ Show (making it printable).

getGoModule :: [String] -> GoModule
-- Pattern matching:
-- Matches with ["something", "something", "something"], and then uses the
-- variables p, v and g to construct a Module.
getGoModule [p, v, g] = GoModule {path = p, version = v, goMod = g}

parseGoModules :: String -> [GoModule]
-- Dot operator (function composition):
-- f(x) . g(x) = f(g(x))
-- (getGoModule . words) x = getGoModule(words(x))
--
-- With map:
-- For every line in (lines c), map calls (getGoModule . words)
parseGoModules c = map (getGoModule . words) (lines c)

data EgoDep = EgoDep
  { name :: String
  , hash :: String
  , uri :: String
  } deriving (Show)

toEgoDep :: GoModule -> EgoDep
-- Again, pattern matching:
-- Matches with the GoModule data, where `path` is set to be the same thing as
-- `n` and `version` is the same thing as `h`.
toEgoDep GoModule {path = n, version = h} = EgoDep {name = n, hash = h, uri = n}

getAbsDirectories :: FilePath -> IO [FilePath]
getAbsDirectories filePath =
  getDirectories filePath >>= mapM (canonicalizePath . (filePath </>))

getDirectories :: FilePath -> IO [FilePath]
getDirectories filePath =
  listDirectory filePath >>= filterM (doesDirectoryExist . (filePath </>))

callCommand :: FilePath -> String -> IO (ExitCode, String, String)
callCommand cwd cmd =
  readCreateProcessWithExitCode (shell cmd) {cwd = Just cwd} []

getLongHash :: FilePath -> String -> IO String
getLongHash path version = do
  (_, hash, _) <- callCommand path ("git rev-parse " ++ version)
  print hash
  return (init hash) -- Drop last newline in rev-parse output.

getGoModHead :: FilePath -> String -> IO String
getGoModHead path version = do
  (e, mod, _) <- callCommand path ("git show " ++ version ++ ":go.mod")
  if e == ExitSuccess
    then return (init mod)
    else return []

findLongHash :: [FilePath] -> String -> String -> IO String
findLongHash [] _ _ = return [] -- No long hash found.
findLongHash (path:paths) name version = do
  mod <- getGoModHead path version
  if name `isSuffixOf` mod
    then getLongHash path version
    else findLongHash paths name version
