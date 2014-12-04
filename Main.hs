{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Char8 as B
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time (LocalTime)
import Data.Time.Format
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import System.Locale (defaultTimeLocale)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

newtype Megabytes = Megabytes Integer  deriving (Eq, Ord)
newtype Upload    = Upload Megabytes   deriving (Eq, Ord, Show)
newtype Download  = Download Megabytes deriving (Eq, Ord, Show)

downloadToInteger :: Download -> Integer
downloadToInteger (Download (Megabytes x)) = x

uploadToInteger :: Upload -> Integer
uploadToInteger (Upload (Megabytes x)) = x

toGB :: Integer -> Double
toGB x = fromIntegral x / 1024

instance Show Megabytes where
  show (Megabytes x) = (show x) ++ " MB"

data BandwidthUsage = BandwidthUsage {
    _monthyear :: LocalTime
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
  return $ BandwidthUsage (readTime defaultTimeLocale "%m-%Y" date) weeks monthTotals

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

graphAll :: B.ByteString -> IO ()
graphAll d = do
  let Right parsed = A.parseOnly parseEverything d
  p <- plot' $ sortBy (comparing _monthyear) parsed
  toFile def "/tmp/dd-wrchart-all.svg" $ do
    layout_title .= "All data"
    layout_y_axis . laxis_title .= "GBs"
    layout_x_axis . laxis_title .= "Months"
    fst p
    snd p
  where
    plot' bw = do
      let dl = map (\x -> (_monthyear x, toGB . downloadToInteger . fst . _totals $ x)) bw
          ul = map (\x -> (_monthyear x, toGB . uploadToInteger . snd . _totals $ x)) bw
      return ( plot (line "Download" [dl])
             , plot (line "Upload" [ul])
             )

main :: IO ()
main = B.readFile "/tmp/f" >>= graphAll
