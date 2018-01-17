{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle
import Filesystem.Path
import Prelude hiding (FilePath)
import IPFSDependencyFile (dependencyFile, ImportName, ImportHash, ImportStatement(Import, hash, name))
import Text.Parsec (ParseError, parse, eof)
import Text.Parsec.String
import Data.Text (unpack, pack, concat)
import Filesystem.Path.CurrentOS


parser :: Turtle.Parser FilePath
parser = argPath "src" "The source file"

getDependencyFile :: FilePath -> FilePath
getDependencyFile hsFile = replaceExtension hsFile "ipfs"

getFileContent :: FilePath -> IO Text
getFileContent path = do
    text <- readTextFile path
    return text

regularParse :: Text.Parsec.String.Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseWithEof :: Text.Parsec.String.Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* Text.Parsec.eof) ""

parseFile :: Text -> [ImportStatement]
parseFile contents = case parseResult of
  Left  _      -> []
  Right result -> result
  where parseResult = regularParse dependencyFile (unpack contents)

getHash :: ImportStatement -> ImportHash
getHash Import { hash=hash } = hash

getName :: ImportStatement -> ImportHash
getName Import { name=name } = name

getHashValues :: [ImportStatement] -> [ImportHash]
getHashValues statements = (map getHash statements)

changeName :: FilePath -> Text -> Maybe FilePath
changeName originalPath newName = case originalPathDirectoryText of
  Left  _    -> Nothing
  Right dir -> Just (fromText (Data.Text.concat [dir, newName]))
  where originalPathDirectoryText = (toText (directory originalPath))

ipfsToFile :: FilePath -> ImportStatement -> IO ()
ipfsToFile srcFile importStatement = let
    dependencyHash = getHash importStatement
    destFile = changeName srcFile (pack ((getName importStatement) ++ ".hs"))
  in
    case destFile of
      Nothing   -> echo "incorrect dest file"
      Just dest -> output dest (inproc "ipfs" ["cat", pack dependencyHash] Turtle.empty)

main = do
    srcFile <- options "A utility for linking haskell dependencies from ipfs" parser
    let dependencyFile = (getDependencyFile srcFile)
    dependencyContents <- readTextFile dependencyFile
    mapM_ id (map (ipfsToFile srcFile) (parseFile dependencyContents))
    -- let dependencyHashes = getHashValues (parseFile dependencyContents)
    -- stdout (inproc "ipfs" ["cat", pack (head dependencyHashes)] Turtle.empty)
