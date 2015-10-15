module Parser (tolerant, strict) where

import Control.Applicative ((*>), (<*), (<$>))
import Control.Monad (void)
import Data.Maybe (catMaybes)
import Data.Word (Word32)
import Numeric (readHex)
import Text.Parsec.Char (endOfLine)
import Text.ParserCombinators.Parsec

eol :: Parser ()
eol = void endOfLine

comment :: Parser ()
comment = do char ';'
             skipMany $ noneOf "\r\n"
          <?> "comment"

-- Is there a better way to do this?
checksum :: Parser Word32
checksum = fst . head . readHex <$> count 8 hexDigit <?> "checksum"

filepath :: Parser FilePath
filepath = (many1 $ alphaNum <|> oneOf ".-") <?> "filepath"

verification :: Parser (FilePath, Word32)
verification = do fp <- filepath
                  many1 $ char ' '
                  c <- checksum
                  return (fp, c)
               <?> "verification"

line :: Parser (Maybe (FilePath, Word32))
line = (const Nothing <$> try comment <|> Just <$> verification) <* (eol <|> eof)

tolerant :: Parser [(FilePath, Word32)]
tolerant = catMaybes <$> (spaces *> many (line <* spaces) <* eof)

strict :: Parser [(FilePath, Word32)]
strict = catMaybes <$> many line <* optional eol <* eof
