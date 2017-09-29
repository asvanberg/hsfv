module Main where

import HSFV (Flags(..), run)

import Control.Monad (join)
import Data.Bifunctor (first)
import Data.Word (Word32)
import Data.Functor ((<$>))
import Data.List.NonEmpty (intersperse, NonEmpty)
import Data.Semigroup (sconcat)
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt
import System.FilePath ((</>), takeDirectory)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Text.ParserCombinators.Parsec (parseFromFile, Parser, ParseError)

main :: IO ()
main = do args <- getArgs
          (flags, sfv) <- parseArgs args
          exitWith =<< HSFV.run flags sfv

parseArgs :: [String] -> IO (Flags, FilePath)
parseArgs args =
  case getOpt RequireOrder options args of
    (o, [sfv], []) -> return (foldl (flip id) defaultOptions o, sfv)
    (_, _, errs)   -> getProgName >>=
      \progName -> ioError . userError $ concat errs ++ usageInfo (header progName) options
  where
    header progName = "Usage: " ++ progName ++ " [OPTION...] file"
    defaultOptions = Flags { tolerant = False, failFast = False }

options :: [OptDescr (Flags -> Flags)]
options =
  [ Option ['t']  ["tolerant"]       (NoArg $ \f -> f { tolerant = True }) "tolerant parsing"
  , Option []     ["ff", "failfast"] (NoArg $ \f -> f { failFast = True }) "stop verifying on first failure"
  ]
