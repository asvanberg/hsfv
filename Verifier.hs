module Verifier (Result(..), Type(..), verify) where

import Control.Exception.Base (handleJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS (readFile)
import Data.Digest.CRC32 (crc32)
import Data.Functor ((<$>))
import Data.Word (Word32)
import System.IO.Error (isDoesNotExistError)

data Result = Result FilePath Type
data Type
  = Ok
  | Missing
  | Invalid Word32 Word32
  deriving (Eq, Show)

calculateChecksum :: FilePath -> IO Word32
calculateChecksum = (crc32 <$>) . BS.readFile

verify :: FilePath -> Word32 -> IO Result
verify fp expected =
  liftIO $ Result fp <$> handleJust
    (\e -> if isDoesNotExistError e then Just Missing else Nothing)
    return
    (do
      actual <- calculateChecksum fp
      return $ if actual == expected then Ok else Invalid expected actual
    )
