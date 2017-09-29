{-# LANGUAGE FlexibleContexts #-}
module Parser (tolerant, strict) where

import Control.Applicative ((*>), (<*), (<$>))
import Control.Monad (void)
import Data.Maybe (catMaybes)
import Data.Word (Word32)
import Numeric (readHex)
import Text.Parsec
import Text.Parsec.Char

data Line = Verification FilePath Word32 | Comment String

eol :: (Stream s f Char) => ParsecT s u f ()
eol = void endOfLine

comment :: (Stream s f Char) => ParsecT s u f ()
comment = do char ';'
             skipMany $ noneOf "\r\n"
          <?> "comment"

-- Is there a better way to do this?
checksum :: (Stream s f Char) => ParsecT s u f Word32
checksum = fst . head . readHex <$> count 8 hexDigit <?> "checksum"

filepath :: (Stream s f Char) => ParsecT s u f FilePath
filepath = many1 (alphaNum <|> oneOf ".-") <?> "filepath"

verification :: (Stream s f Char) => ParsecT s u f (FilePath, Word32)
verification = do fp <- filepath
                  many1 $ char ' '
                  c <- checksum
                  return (fp, c)
               <?> "verification"

line :: (Stream s f Char) => ParsecT s u f (Maybe (FilePath, Word32))
line = (const Nothing <$> try comment <|> Just <$> verification) <* (eol <|> eof)

tolerant :: (Stream s f Char) => ParsecT s u f [(FilePath, Word32)]
tolerant = catMaybes <$> (spaces *> many (line <* spaces) <* eof)

strict :: (Stream s f Char) => ParsecT s u f [(FilePath, Word32)]
strict = catMaybes <$> many line <* optional eol <* eof
