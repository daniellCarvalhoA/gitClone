{-# LANGUAGE OverloadedStrings #-}
module ParseCommandLine (parseCommandLine, Host) where

import Options.Applicative
import Data.Text 
import Control.Exception
import System.Directory (doesDirectoryExist)
import Data.Typeable    (Typeable)
import ParseHost        (hostParser, Host, HostParseError(..))


data ParseExceptions = UnsupportedProtocol 
                     | PathMissing 

instance Show ParseExceptions where
  show UnsupportedProtocol = "At the moment only dumb and smart htttp are provided" 
  show PathMissing         = "Path missing fromurl"

instance Exception ParseExceptions

data DirectoryDoesNotExist a = DirectoryDoesNotExist a

instance Show a => Show (DirectoryDoesNotExist a) where
  show (DirectoryDoesNotExist x) = "The direcory " <> show x <> " does not exist on the filesystem" 

instance (Typeable a, Show a) => Exception (DirectoryDoesNotExist a)


data Config = Config {
    host :: Text
  , dir  :: Text 
  } deriving Show

configParser :: Parser Config 
configParser = 
  Config 
    <$> strArgument (metavar "host" <> help "Repository to clone.")
    <*> strArgument (value "."      <> metavar "directory" <> help "where to clone.")

opts :: ParserInfo Config 
opts = info (configParser <**> helper) 
  (fullDesc <> progDesc "Limited git clone" <> header "test for git clone")

parseCommandLine :: IO Host
parseCommandLine = do 
  Config repo d <- execParser opts
  b <- doesDirectoryExist $ unpack d
  if b 
  then either (throwIO . hostParseException) pure $ hostParser repo
  else throwIO $ DirectoryDoesNotExist d
    where
        hostParseException :: HostParseError -> ParseExceptions
        hostParseException Unsupported    = UnsupportedProtocol 
        hostParseException UrlPathMissing = PathMissing         
 
