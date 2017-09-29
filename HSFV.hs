module HSFV (Flags(..), run) where

import qualified Parser
import Verifier

import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Bool (bool)

import Numeric (showHex)

import System.Exit (exitWith, ExitCode(..))
import System.FilePath ((</>), takeDirectory)

import Text.Parsec.ByteString (parseFromFile)

data Flags = Flags
  { tolerant :: Bool
  , failFast :: Bool
  }

run :: Flags -> FilePath -> IO ExitCode
run flags sfv =
  do
    parseResult <- parseFromFile (if tolerant flags then Parser.tolerant else Parser.strict) sfv
    case parseResult of
      Left parseError ->
        do putStrLn "Parsing failure;"
           print parseError
           return $ ExitFailure 2

      Right checksums ->
        let
          sfvDirectory = takeDirectory sfv
          updatedPaths = first (sfvDirectory </>) <$> checksums

          showResult result =
            case result of
              Result fp Ok ->
                putStrLn $ fp ++ " ... Ok"
              Result fp Missing ->
                putStrLn $ fp ++ " ... Missing"
              Result fp (Invalid expected actual) ->
                putStrLn $ fp ++ " ... Failed (expected: " ++ showHex expected "" ++ ", actual: " ++ showHex actual "" ++ ")"

          processResults valid [] =
            return valid
          processResults valid (x:xs) =
            do
              showResult x
              if shouldContinue x
                then processResults (valid && isValid x) xs
                else return False

          shouldContinue (Result _ t) =
            t == Ok || not (failFast flags)
        in
          bool (ExitFailure 1) ExitSuccess <$> (processResults True =<< traverse (uncurry Verifier.verify) updatedPaths)

isValid :: Result -> Bool
isValid (Result _ typ) = typ == Ok
