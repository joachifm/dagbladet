#!/usr/bin/env runghc

A stupid daemon for continually scraping
content from the front page of dagbladet.no.

\section{Module header}

\begin{code}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import qualified Data.ByteString.Char8 as SB

import Control.Applicative
import Control.Monad (forever, unless)
import Control.Concurrent (threadDelay)
import System.Process (system)
import System.Directory (doesFileExist)

import Data.Time.Clock
import Data.Time.Calendar

\end{code}

\section{Main loop}

\begin{code}

main = forever $ do
  firstTime <- isFirstTime
  unless firstTime $ do
    logConsole "Waiting ..."
    sleep =<< getNextTime
  logConsole "Getting ..."
  system "./dagbladet get"
  writeLastTime

logConsole msg = do
  now <- getCurrentTime
  print now >> putStrLn msg

-- Like posix sleep
sleep :: Int -> IO ()
sleep secs = threadDelay (secs * 1000000)

-- How many seconds until next fetch?
getNextTime = nextTime <$> getLastTime <*> getCurrentTime

nextTime t0 t1 = norm $ round (minTimeDiff - diff)
  where
    diff = t1 `diffUTCTime` t0

    norm n | n < 0     = 0 -- we are "overdue"
           | otherwise = n

-- Have to wait at least this many seconds
minTimeDiff = 3600

\end{code}

\section{Logging helpers}
\begin{code}

isFirstTime = not `fmap` doesFileExist logFile

writeLastTime = do
  now <- getCurrentTime
  SB.writeFile logFile (encodeUTC now)

getLastTime = decodeUTC `fmap` SB.readFile logFile

logFile = "./last"

\end{code}

\section{Encoding and decoding UTC values}

\begin{code}

encodeUTC = SB.pack . show . fromUTC

decodeUTC = toUTC . read . SB.unpack

\end{code}

\begin{code}

fromUTC :: UTCTime -> (Integer, Rational)
fromUTC t = (toModifiedJulianDay $ utctDay t, toRational $ utctDayTime t)

toUTC :: (Integer, Rational) -> UTCTime
toUTC (dd, dt) = UTCTime (ModifiedJulianDay dd) (fromRational dt)

\end{code}
