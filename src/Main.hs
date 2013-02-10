module Main (main) where

import Dagbladet.Headline

import Network.Download (openURI)

------------------------------------------------------------------------

main :: IO ()
main = do
  c <- openURI "http://www.dagbladet.no"
  case c of
    Left e  -> fail e
    Right r -> print (parseHeadlines r)
