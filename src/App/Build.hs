module App.Build (build) where

import App.Config
import Dagbladet.Headline

import Data.List (nub, sort)

import qualified Data.ByteString as SB
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Directory
import System.FilePath

------------------------------------------------------------------------

-- | Scan cached source and build a corpus of headlines.
build :: AppConfig -> IO ()
build cfg = do
  cs <- mapM SB.readFile =<< getChildren (appCacheDir cfg)
  let dest = appDataDir cfg </> "corpus"
      hl = sort . nub . concatMap parseHeadlines $ cs
      ot = fmt hl
  T.writeFile dest ot

------------------------------------------------------------------------

fmt :: [Headline] -> T.Text
fmt = T.unlines . map fmt1

fmt1 :: Headline -> T.Text
fmt1 x = record [ hPubDate x, hUrl x, hText x ]

record :: [T.Text] -> T.Text
record = T.intercalate (T.pack ",")

------------------------------------------------------------------------

getChildren :: FilePath -> IO [FilePath]
getChildren root =
  (map (root </>) . filter (`notElem` [".", ".."]))
  `fmap` getDirectoryContents root
