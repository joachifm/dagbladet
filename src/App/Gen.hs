module App.Gen where

import App.Config

import Control.Applicative
import Data.List
import Data.MarkovChain
import System.Random

import qualified Data.ByteString as SB
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.FilePath
import System.Directory

------------------------------------------------------------------------

main :: AppConfig -> IO ()
main conf = T.putStr . T.unlines =<< (generateHeadlinesN 10 <$> getHeadlines conf <*> newStdGen)

------------------------------------------------------------------------

getHeadlines :: AppConfig -> IO [T.Text]
getHeadlines conf =
  extractHeadlines <$> T.readFile (appDataDir conf </> "corpus")

extractHeadlines :: T.Text -> [T.Text]
extractHeadlines = map ((!! 2) . T.split (== ',')) . T.lines

------------------------------------------------------------------------

generateHeadlinesN :: RandomGen g => Int -> [T.Text] -> g -> [T.Text]
generateHeadlinesN n xs = take n . generateHeadlines xs

generateHeadlines :: RandomGen g => [T.Text] -> g -> [T.Text]
generateHeadlines xs =
  nub . filter (`notElem` xs) . map T.unwords . runMulti 2 (map T.words xs) 0
