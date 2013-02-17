{-# LANGUAGE DeriveDataTypeable #-}

module App.Gen where

import App.Config

import Control.Applicative
import Data.List
import Data.MarkovChain
import System.Random

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.FilePath

import System.Environment
import System.Console.CmdArgs

------------------------------------------------------------------------

data GenOpts = GenOpts
  { number :: Int
  , context :: Int
  } deriving (Show, Data, Typeable)

------------------------------------------------------------------------

genOpts :: Mode (CmdArgs GenOpts)
genOpts = cmdArgsMode $ GenOpts
  { number = 10
           &= help "Number of headlines to print"
           &= typ "NUM"
  , context = 2
            &= help "Prediction context"
            &= typ "NUM"
  }
  &= summary "Generate random headlines"
  &= program "gen"

------------------------------------------------------------------------

main :: AppConfig -> IO ()
main conf = do
  opts <- subArgs (cmdArgsRun genOpts)
  T.putStr . T.unlines =<< (gen opts <$> getHeadlines conf <*> newStdGen)

subArgs :: IO a -> IO a
subArgs f = getArgs >>= \as -> withArgs (drop 1 as) f

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
    noBoring = filter (`notElem` xs)
