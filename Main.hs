module Main where

import HSFV (Flags(..), run)

import System.Environment (getArgs, getProgName)
import System.Console.GetOpt
import System.Exit (exitWith)

main :: IO ()
main = getArgs >>= parseArgs >>= uncurry HSFV.run >>= exitWith

parseArgs :: [String] -> IO (Flags, FilePath)
parseArgs args =
  case getOpt RequireOrder options args of
    (o, [sfv], []) -> return (foldl (flip id) defaultOptions o, sfv)
    (_, _, errs)   -> ioError . userError =<< fmap
      (\progName -> concat errs ++ usageInfo (header progName) options)
      getProgName
  where
    header progName = "Usage: " ++ progName ++ " [OPTION...] file"
    defaultOptions = Flags { tolerant = False, failFast = False }

options :: [OptDescr (Flags -> Flags)]
options =
  [ Option ['t']  ["tolerant"]       (NoArg $ \f -> f { tolerant = True }) "tolerant parsing"
  , Option []     ["ff", "failfast"] (NoArg $ \f -> f { failFast = True }) "stop verifying on first failure"
  ]
