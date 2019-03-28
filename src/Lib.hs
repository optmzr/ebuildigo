-- My try at learning Haskell with some stdin parsing program.
module Lib where

import Control.Monad (filterM)
import Data.List (isPrefixOf, isSuffixOf)
import GHC.IO.Exception (ExitCode(ExitSuccess))
import System.Directory
  ( canonicalizePath
  , doesDirectoryExist
  , getHomeDirectory
  , listDirectory
  )
import System.Environment (lookupEnv)
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
  } deriving (Show) -- GoModule deriving _from_ Show (making it printable).

getGoModule :: [String] -> GoModule
-- Pattern matching:
-- Matches with ["something", "something"], and then uses the
-- variables p and v to construct a Module.
getGoModule [p, v] = GoModule {path = p, version = v}

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
  , rev :: String
  , ref :: String
  } deriving (Show)

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
  (e, hash, _) <- callCommand path ("git rev-parse " ++ version)
  if e == ExitSuccess
    then return $ init hash -- Drop last newline in rev-parse output.
    else return ""

getGoModHead :: FilePath -> String -> IO String
getGoModHead path version = do
  (e, mod, _) <- callCommand path ("git show " ++ version ++ ":go.mod")
  if e == ExitSuccess
    then return $ last $ words $ init mod -- Extract last word w/o newline.
    else return "" -- Nothing found for version or go.mod doesn't exist.

getGitRemote :: FilePath -> IO String
getGitRemote path = do
  (e, remote, _) <- callCommand path "git remote get-url origin"
  if e == ExitSuccess
    then return $ init remote -- Drop last newline in get-url output.
    else return ""

getGoPath :: IO FilePath
getGoPath = do
  (e, path, _) <- callCommand "/" "go env GOPATH"
  if e == ExitSuccess
    then return $ init path -- Drop last newline in go env output.
    else do
      home <- getHomeDirectory
      return $ home </> "go"

findGoModule :: [FilePath] -> String -> String -> IO (FilePath, String, String)
findGoModule [] _ _ = return ([], [], []) -- No Go module found.
findGoModule (path:paths) name version = do
  mod <- getGoModHead path version
  hash <- getLongHash path version
  remote <- getGitRemote path
  if name `isSuffixOf` mod ||
     version `isPrefixOf` hash || name `isSuffixOf` remote
    then return (path, hash, remote)
    else findGoModule paths name version

versionToRev :: String -> String
versionToRev version
  | "+incompatible" `isSuffixOf` version = take (length version - 13) version
  | length version == 34 = drop 22 version
  | otherwise = version

chooseRev :: String -> String -> String
chooseRev rev hash
  | hash /= "" = hash
  | otherwise = rev

toRef :: String -> String
toRef = drop 8

toEgoDep :: GoModule -> IO EgoDep
-- Again, pattern matching:
-- Matches with the GoModule data, where `path` is set to be the same thing as
-- `name` and `version` is the same thing as `version`.
toEgoDep GoModule {path = name, version = version} = do
  let rev = versionToRev version
  gopath <- getGoPath
  dirs <- getAbsDirectories $ gopath </> "pkg/mod/cache/vcs/"
  (dir, hash, remote) <- findGoModule dirs name rev
  return EgoDep {name = name, rev = chooseRev rev hash, ref = toRef remote}

printEgoDep :: EgoDep -> IO ()
printEgoDep dep = putStrLn $ n ++ rv ++ rf
  where
    n = name dep
    rv = " " ++ rev dep
    rf
      | n /= ref dep = " " ++ ref dep
      | otherwise = ""
