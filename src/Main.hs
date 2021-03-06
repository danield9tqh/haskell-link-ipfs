{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Turtle
import IPFSDependencyFile (dependencyFile, ImportName, ImportHash, ImportStatement(Import, hash, name))
import Text.Parsec (ParseError, parse, eof)
import qualified Text.Parsec.String
import Data.Text (unpack, pack, concat, Text)
import Data.Either (fromRight)
import Filesystem.Path.CurrentOS as FS (directory, toText, fromText, replaceExtension, FilePath)

data DependencyFileResult = Error ParseError | Result [ImportStatement]

parser :: Turtle.Parser FS.FilePath
parser = Turtle.argPath "src" "The source file"

getDependencyFile :: FS.FilePath -> FS.FilePath
getDependencyFile hsFile = replaceExtension hsFile "ipfs"

getFileContent :: FS.FilePath -> IO Turtle.Text
getFileContent path = do
    text <- Turtle.readTextFile path
    return text

parseWithEof :: Text.Parsec.String.Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* Text.Parsec.eof) ""

parseFile :: Turtle.Text -> DependencyFileResult
parseFile contents = case parseResult of
  Left  e      -> Error e
  Right result -> Result result
  where parseResult = parseWithEof dependencyFile (unpack contents)

getHash :: ImportStatement -> ImportHash
getHash Import { hash=hash } = hash

getName :: ImportStatement -> ImportHash
getName Import { name=name } = name

getHashValues :: [ImportStatement] -> [ImportHash]
getHashValues statements = (map getHash statements)

changeName :: FS.FilePath -> Turtle.Text -> Maybe FS.FilePath
changeName originalPath newName = case originalPathDirectoryText of
  Left  _    -> Nothing
  Right dir -> Just (fromText (Data.Text.concat [dir, newName]))
  where originalPathDirectoryText = (toText (directory originalPath))

ipfsToFile :: FS.FilePath -> ImportStatement -> IO ()
ipfsToFile srcFile importStatement = let
    dependencyHash = getHash importStatement
    destFile = changeName srcFile (pack ((getName importStatement) ++ ".hs"))
  in
    case destFile of
      Nothing   -> Turtle.echo "incorrect dest file"
      Just dest -> do
        print ("getting hash from ipfs " ++ dependencyHash ++ " -> " ++ ((unpack . fromRight "" . toText) dest))
        Turtle.output dest (Turtle.inproc "ipfs" ["cat", pack dependencyHash] Turtle.empty)

main = do
    srcFile <- Turtle.options "A utility for linking haskell dependencies from ipfs" parser
    let dependencyFile = (getDependencyFile srcFile)
    dependencyContents <- Turtle.readTextFile dependencyFile
    let parsedDepFile = parseFile dependencyContents
    case parsedDepFile of
      Error e  -> print e
      Result parsedDepFile -> mapM_ id (map (ipfsToFile srcFile) parsedDepFile)
