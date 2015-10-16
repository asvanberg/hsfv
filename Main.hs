module Main where

import Parser (strict, tolerant)
import Verifier (verifySfv)

import Data.Word (Word32)
import Data.Functor ((<$>))
import Data.List.NonEmpty (intersperse, NonEmpty)
import Data.Semigroup (sconcat)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Text.ParserCombinators.Parsec (parseFromFile, Parser, ParseError)

data SfvError = InvalidSfv ParseError | VerificationFailed (NonEmpty FilePath) deriving (Show)

parseAndVerify :: Parser [(FilePath, Word32)] -> FilePath -> IO (Either SfvError ())
parseAndVerify parser sfvFile =
  do parsed <- parseFromFile parser sfvFile
     either parseError parseResult (verifySfv <$> parsed)
  where parseError = return . Left . InvalidSfv
        parseResult = fmap $ either (Left . VerificationFailed) Right

main :: IO ()
main = do args <- getArgs
          case args of
            ["-t", sfvFile] -> f tolerant sfvFile
            [sfvFile] -> f strict sfvFile
            _ -> do progName <- getProgName
                    putStrLn $ "Usage: " ++ progName ++ " [-t] <sfv>"
                    putStrLn "Use -t flag for tolerant parsing"
       where f parser sfvFile = parseAndVerify parser sfvFile >>= either showError success
             success = const $ putStrLn "Ok"
             showError (InvalidSfv pe) =
               do putStrLn "Parsing failure;"
                  print pe
                  exitWith (ExitFailure 1)
             showError (VerificationFailed failedFiles) =
               do putStrLn "Verification failure;"
                  putStrLn $ sconcat $ intersperse "\n" $ show <$> failedFiles
                  exitWith (ExitFailure 2)
