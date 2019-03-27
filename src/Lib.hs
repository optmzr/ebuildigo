module Lib where

-- My try at learning Haskell with some stdin parsing program.
import System.IO

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
