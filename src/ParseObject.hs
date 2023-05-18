{-# LANGUAGE OverloadedStrings #-}
module ParseObject (
          commitParser
        , Commit(..)
        , treeParser
        , Tree(..)
        , blobParser
        , Blob(..)
        , Entry(..)
        , Mode(..)
        ) where

import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString                  as B
import qualified Data.ByteString as C
import Data.Functor (void)
import Data.ByteString.Base16                       (decode)
import Control.Applicative ((<|>))

-------------------------------------------------------- Commit Parser 

data Commit = Commit {
    treeid    :: B.ByteString
  , parents   :: [B.ByteString]
  , cAuthor   :: Identity
  , cCommiter :: Identity
  , cMessage  :: B.ByteString
  } deriving Show

type Name      = C.ByteString
type Email     = C.ByteString
type TimeStamp = C.ByteString
type TimeZone  = C.ByteString

data Identity  = Author    Name Email TimeStamp TimeZone
               | Committer Name Email TimeStamp TimeZone
    deriving Show

commitParser :: B.ByteString -> Either String Commit
commitParser = AC.parseOnly parseCommit

parseCommit :: AC.Parser Commit
parseCommit = do
  _ <- AC.string "tree" <* AC.space
  s <- AC.take 40       <* AC.space
  case decode s of
    Left m -> fail m
    Right sh -> do
      p <- AC.many' parseParent
      a <- AC.string "author " *> parseId Author
      c <- AC.string "committer" *> AC.space *> parseId Committer
      void AC.space
      Commit sh p a c <$> AC.takeByteString

parseParent :: AC.Parser C.ByteString
parseParent = AC.string "parent" *> AC.space *> AC.take 40 <* AC.space

parseId :: (Name -> Email -> TimeStamp -> TimeZone -> Identity) -> AC.Parser Identity
parseId f = do
  n  <- AC.takeTill (== '<')   <* AC.char '<'
  e  <- AC.takeTill (== '>')   <* AC.char '>'
  ts <- AC.space *> AC.takeWhile AC.isDigit <* AC.space
  tz <- AC.takeTill (== '\n' ) <* AC.char '\n'
  return $ f (B.dropEnd 1 n) e ts tz --  ts tz

----------------------------------------------- Tree Parser 

newtype Tree = MkTree { mktree :: [Entry] } 
  deriving Show

data Entry = Entry {
    mode :: Mode
  , path :: C.ByteString
  , obid :: C.ByteString 
  } deriving Show

data Mode = Dir | File | Exec | Link | GitLink
  deriving (Eq, Show)

treeParser :: B.ByteString -> Either String Tree 
treeParser = AC.parseOnly parseTree

parseTree :: AC.Parser Tree 
parseTree = MkTree <$> AC.many1' parseEntry 

parseEntry :: AC.Parser Entry
parseEntry = do 
  m <- Dir     <$ AC.string "40000" 
   <|> GitLink <$ AC.string "160000" 
   <|> File    <$ AC.string "100644" 
   <|> Exec    <$ AC.string "100755"
   <|> Link    <$ AC.string "120000"
  void AC.space
  p <- AC.takeTill (== '\0') <* AC.char '\0'
  b <- AC.take 20 
  return $ Entry  m p b  
  
----------------------------------------------- Blob Parser 

newtype Blob = MkBlob { mkBlob :: C.ByteString }

blobParser :: B.ByteString -> Either String Blob
blobParser = AC.parseOnly (MkBlob <$> AC.takeByteString)











