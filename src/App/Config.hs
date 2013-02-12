module App.Config (AppConfig(..), defaultConfig) where

------------------------------------------------------------------------

data AppConfig = AppConfig
  { appCacheDir :: FilePath
  , appDataDir :: FilePath
  , appConfDir :: FilePath
  } deriving (Read, Show)

------------------------------------------------------------------------

defaultConfig :: AppConfig
defaultConfig = AppConfig
  { appCacheDir = "./cache"
  , appDataDir = "./dataDir"
  , appConfDir = "./confDir"
  }
