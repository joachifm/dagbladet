#!/usr/bin/env runghc

A stupid daemon for continually scraping
content from the front page of dagbladet.no.

\section{Module header}

\begin{code}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import qualified Data.ByteString.Char8 as SB

import Control.Applicative
import Control.Monad (forever, unless, void)
import System.Posix.Unistd (sleep)
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
    nsecs <- getNextTime
    void (sleep nsecs)
  system "./dagbladet get"
  writeLastTime

-- How many seconds until next fetch?
getNextTime = do
  now <- getCurrentTime
  lastTime <- getLastTime
  return $ nextTime lastTime now

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
