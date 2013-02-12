module Dagbladet.Headline (Headline(..), parseHeadlines) where

import           Data.Char
import qualified Data.Text                as T
import qualified Data.ByteString.Char8    as B

import Text.HTML.TagSoup

------------------------------------------------------------------------

data Headline = Headline
  { hPubDate :: T.Text
  , hUrl :: T.Text
  , hText :: T.Text
  } deriving Show

instance Eq Headline where
  x == y = hText x == hText y

instance Ord Headline where
  x `compare` y =
    case hPubDate x `compare` hPubDate y of
      EQ -> hText x `compare` hText y
      r  -> r

------------------------------------------------------------------------

-- | Parse headlines from page source.
parseHeadlines :: B.ByteString -> [Headline]
parseHeadlines = map fromA . headlineTags . parseTags

------------------------------------------------------------------------

-- Construct headline from headline link.
fromA :: Tag B.ByteString -> Headline
fromA x =
  let url = attr "href" x
  in Headline { hPubDate = parseUrlDate url
              , hUrl = url
              , hText = onespace (attr "title" x)
              }

-- Extract attribute value as a properly encoded 'Text' value.
attr :: String -> Tag B.ByteString -> T.Text
attr x = decodeLatin1 . fromAttrib (B.pack x)

------------------------------------------------------------------------

-- Replace all whitespace with a single space (also removes trailing whitespace).
onespace :: T.Text -> T.Text
onespace = T.unwords . T.words

------------------------------------------------------------------------

-- Extract date portion from headline link.
parseUrlDate :: T.Text -> T.Text
parseUrlDate = T.init . T.takeWhile (not . isLetter)
             . T.dropWhile (not . isDigit)

------------------------------------------------------------------------

-- Extract headline anchors from a soup of tags.
headlineTags :: [Tag B.ByteString] -> [Tag B.ByteString]
headlineTags = map f . sections (~== "<h2>")
             . takeWhile (~/= "<style type='text/css'>")
             . dropWhile (~/= "<div id='content'>")
  where
    f x = sections (~== "<a>") x !! 0 !! 0

------------------------------------------------------------------------

decodeLatin1 :: B.ByteString -> T.Text
decodeLatin1 = T.pack . B.unpack
