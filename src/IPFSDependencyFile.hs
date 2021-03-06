module IPFSDependencyFile (dependencyFile, ImportName, ImportHash, ImportStatement(Import, hash, name)) where

import Text.Parsec.Char (string, satisfy)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1, optional)
import Text.Parsec.Prim (many)
import Data.Char (isNumber, isLetter, isUpper)

type ImportName = String
type ImportHash = String

data ImportStatement = Import { name :: ImportName, hash :: ImportHash } deriving (Show)

isLetterOrNumber :: Char -> Bool
isLetterOrNumber c = (isLetter c) || (isNumber c)

-- TODO: Allow '.' in import name
importName :: Parser ImportName
importName = do
  first <- satisfy isUpper
  rest <- many (satisfy isLetterOrNumber)
  return (first:rest)

importHash :: Parser ImportHash
importHash = do
  hash <- many1 (satisfy isLetterOrNumber)
  return hash

importStatement :: Parser ImportStatement
importStatement = do
  name <- importName
  string ": "
  hash <- importHash
  return Import {name=name, hash=hash}

nonTerminalImportStatement :: Parser ImportStatement
nonTerminalImportStatement = do
  statement <- importStatement
  optional (string "\n")
  return statement

dependencyFile :: Parser [ImportStatement]
dependencyFile = do
  statements <- many nonTerminalImportStatement
  return statements
