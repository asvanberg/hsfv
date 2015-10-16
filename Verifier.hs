module Verifier (verifySfv) where

import Control.Monad (filterM)
import qualified Data.ByteString.Lazy as BL (readFile)
import Data.Digest.CRC32 (crc32)
import Data.Functor ((<$>))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Word (Word32)

verify :: FilePath -> Word32 -> IO Bool
verify fp c = (c ==) <$> crc32 <$> BL.readFile fp

invalid :: FilePath -> Word32 -> IO Bool
invalid fp c = not <$> verify fp c

verifySfv :: [(FilePath, Word32)] -> IO (Either (NonEmpty FilePath) ())
verifySfv verifications = do
  failed <- filterM (uncurry invalid) verifications
  return $ maybe (Right ()) Left $ nonEmpty $ fst <$> failed
