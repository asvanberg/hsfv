module Main where

import HSFV (Flags(..), run)

import Control.Monad (join)
import Data.Bifunctor (first)
import Data.Word (Word32)
import Data.Functor ((<$>))
import Data.List.NonEmpty (intersperse, NonEmpty)
import Data.Semigroup (sconcat)
import System.Environment (getArgs, getProgName)
import System.FilePath ((</>), takeDirectory)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Text.ParserCombinators.Parsec (parseFromFile, Parser, ParseError)

main :: IO ()
main = do args <- getArgs
          exitWith =<< case parseArgs args of
            Nothing -> do
              progName <- getProgName
              putStrLn $ "Usage: " ++ progName ++ " [-t] [-ff] <sfv>"
              putStrLn "Use -t flag for tolerant parsing"
              putStrLn "Use -ff flag for fail fast behaviour"
              return $ ExitFailure 0
            Just (flags, sfv) ->
              HSFV.run flags sfv

parseArgs :: [String] -> Maybe (Flags, FilePath)
parseArgs =
  let
    parse _     []          = Nothing
    parse flags [sfv]       = Just (flags, sfv)
    parse flags (flag:rest) = parse (update flags flag) rest

    update flags flag =
      case flag of
        "-t" ->
          flags { tolerant = True }
        "-ff" ->
          flags { failFast = True }
        _ ->
          flags
  in
    parse Flags { tolerant = False, failFast = False }
