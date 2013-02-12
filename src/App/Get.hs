module App.Get (get) where

import           App.Config

import qualified Data.ByteString      as SB
import qualified Codec.Digest.SHA     as SHA
import           Codec.Digest.SHA (Length(..))
import           System.FilePath
import           Network.Download (openURI)

------------------------------------------------------------------------

-- | Download front page source to cache for further processing.
get :: AppConfig -> IO ()
get cfg = do
  c <- openURI "http://www.dagbladet.no"
  case c of
    Left e  -> fail e
    Right r -> do
      let name = SHA.showBSasHex $ SHA.hash SHA256 r
          dest = appCacheDir cfg </> name
      SB.writeFile dest r
