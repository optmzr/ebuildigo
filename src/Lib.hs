module Lib where

-- My try at learning Haskell with some stdin parsing program.
import System.IO

data Module = Module
  { path :: String
  , version :: String
  , goMod :: String
  } deriving (Show)

getModule :: [String] -> Module
getModule [p, v, g] = Module {path = p, version = v, goMod = g}

parseModules :: String -> [Module]
-- Dot operator (function composition):
-- f(x) . g(x) = f(g(x))
-- (getModule . words) x = getModule(words(x))
--
-- With map:
-- For every line in (lines c), map calls (getModule . words)
parseModules c = map (getModule . words) (lines c)
