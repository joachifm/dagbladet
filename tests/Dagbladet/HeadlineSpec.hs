module Dagbladet.HeadlineSpec (spec) where

import Dagbladet.Headline
import qualified Data.Text as T

import Test.Hspec

------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "parseUrlDate" $ do
    it "extracts the date part from a frontpage URL" $
      let uri = "http://www.dagbladet.no/2013/02/10/category/title/tag1/tag2/25660342/"
      in parseUrlDate (T.pack uri) == T.pack "2013/02/10"
    it "returns an empty result on unrecognised uri patterns" $
      parseUrlDate (T.pack "http://www.example.com/foo/bar/") == T.pack ""
