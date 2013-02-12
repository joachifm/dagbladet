module Main (main) where

import App.Config
import App.Get

import System.Environment (getArgs)

------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  let conf = defaultConfig
  case args of
    ("get":_) -> get conf
