module App.Build (build) where

import App.Config
import Dagbladet.Headline

import Control.Applicative
import Data.List (nub, sort)

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Directory
import System.FilePath

------------------------------------------------------------------------

-- | Scan cached source and build a corpus of headlines.
build :: AppConfig -> IO ()
build cfg =
  getHeadlines (appCacheDir cfg) >>=
  writeHeadlines (appDataDir cfg </> "corpus")

------------------------------------------------------------------------

getHeadlines :: FilePath -> IO [Headline]
getHeadlines rootDir =
  concatMap parseHeadlines <$> (mapM readCached =<< getChildren rootDir)

readCached :: FilePath -> IO SB.ByteString
readCached fileName = strictify . GZip.decompress <$> LB.readFile fileName
  where strictify = SB.concat . LB.toChunks

writeHeadlines :: FilePath -> [Headline] -> IO ()
writeHeadlines fileName =
  T.writeFile fileName . T.unlines . map fmt1 . sort . nub

------------------------------------------------------------------------

fmt1 :: Headline -> T.Text
fmt1 x = record [ hPubDate x, hUrl x, hText x ]

record :: [T.Text] -> T.Text
record = T.intercalate (T.pack ",")

------------------------------------------------------------------------

getChildren :: FilePath -> IO [FilePath]
getChildren root =
  (map (root </>) . filter (`notElem` [".", ".."]))
  `fmap` getDirectoryContents root
