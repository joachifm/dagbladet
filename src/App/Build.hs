module App.Build (build) where

import App.Config
import Dagbladet.Headline

import Data.Ord (comparing)
import Data.List (sortBy, nubBy)

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
      hl = sortHeadlines $ concatMap parseHeadlines cs
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

sortHeadlines :: [Headline] -> [Headline]
sortHeadlines = nubBy (\x y -> hText x == hText y)
              . sortBy (comparing hPubDate)

------------------------------------------------------------------------

getChildren :: FilePath -> IO [FilePath]
getChildren root =
  (map (root </>) . filter (`notElem` [".", ".."]))
  `fmap` getDirectoryContents root
