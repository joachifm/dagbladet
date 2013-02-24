module App.Gen where

import App.Config

import Control.Applicative
import Data.List
import Data.MarkovChain
import System.Random

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.FilePath

------------------------------------------------------------------------

data GenOpts = GenOpts
  { number :: Int
  , context :: Int
  } deriving Show

------------------------------------------------------------------------

main :: AppConfig -> IO ()
main conf = do
  let opts = GenOpts 10 3
  T.putStr . T.unlines =<< (gen opts <$> getHeadlines conf <*> newStdGen)

------------------------------------------------------------------------

getHeadlines :: AppConfig -> IO [T.Text]
getHeadlines conf =
  extractHeadlines <$> T.readFile (appDataDir conf </> "corpus")

extractHeadlines :: T.Text -> [T.Text]
extractHeadlines = map ((!! 2) . T.split (== ',')) . T.lines

------------------------------------------------------------------------

gen :: RandomGen g => GenOpts -> [T.Text] -> g -> [T.Text]
gen opts xs g = take (number opts) . nub . noBoring
              . map T.unwords
              . (\xs' -> runMulti (context opts) xs' 0 g)
              $ map T.words xs
  where
    noBoring = filter (\y -> not $ any (T.isPrefixOf y) xs)
