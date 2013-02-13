module Main (main) where

import App.Config
import App.Get
import App.Build
import qualified App.Gen as Gen

import System.Environment (getArgs)

------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  let conf = defaultConfig
  case args of
    ("get":_)   -> get conf
    ("build":_) -> build conf
    ("gen":_)   -> Gen.main conf
