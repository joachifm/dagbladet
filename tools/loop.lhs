#!/usr/bin/env runghc
\begin{code}
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import System.Process (system)
import System.Random (randomRIO)

main :: IO ()
main = forever $ do
  system "./dagbladet get && ./dagbladet build"
  mods <- randomRIO (0, 60)
  threadDelay $ ((60 + mods) * 60) * 1000000
\end{code}
