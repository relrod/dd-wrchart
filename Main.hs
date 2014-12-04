{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Char8 as B
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

newtype Megabytes = Megabytes Integer  deriving (Eq, Ord)
newtype Upload    = Upload Megabytes   deriving (Eq, Ord, Show)
newtype Download  = Download Megabytes deriving (Eq, Ord, Show)

instance Show Megabytes where
  show (Megabytes x) = (show x) ++ " MB"

data BandwidthUsage = BandwidthUsage {
    _monthyear :: B.ByteString -- TODO: Use a real date type
  , _weekdata  :: [(Download, Upload)]
  , _totals    :: (Download, Upload)
  } deriving (Eq, Ord, Show)

makeLenses ''Megabytes
makeLenses ''Upload
makeLenses ''Download
makeLenses ''BandwidthUsage

parseLine :: A.Parser BandwidthUsage
parseLine = do
  _ <- string "traff-"
  date <- manyTill (choice $ [digit, char '-']) (try . char $ '=')
  weeks <- some parseWeek
  monthTotals <- parseTotals
  _ <- newline
  return $ BandwidthUsage (B.pack date) weeks monthTotals

parseWeek :: A.Parser (Download, Upload)
parseWeek = do
  download <- integer
  _ <- colon
  upload <- integer
  return $ (Download . Megabytes $ download, Upload . Megabytes $ upload)

parseTotals :: A.Parser (Download, Upload)
parseTotals = do
  _ <- char '['
  download <- integer
  _ <- colon
  upload <- integer
  _ <- char ']'
  return $ (Download . Megabytes $ download, Upload . Megabytes $ upload)

parseEverything :: A.Parser [BandwidthUsage]
parseEverything = do
  _ <- string "TRAFF-DATA"
  _ <- spaces
  manyTill parseLine eof

main :: IO ()
main = error "todo"
