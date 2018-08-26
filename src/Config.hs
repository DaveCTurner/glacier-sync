{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Config where

import           Control.Exception    (Exception, throwIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict  as HM

data Config = Config
  { cfgMirrorPath :: FilePath
  } deriving (Show, Eq)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .: "mirror_path"

instance ToJSON Config where
  toJSON Config{..} = object
    [ "mirror_path" .= cfgMirrorPath
    ]

defaultConfig :: Config
defaultConfig = Config
  { cfgMirrorPath = ""
  }

-- recursively overwrite parts of the first value with the corresponding parts of the second
mergeValues :: Value -> Value -> Value
mergeValues (Object hm1) (Object hm2) = Object $ HM.filter (/= Null) $ HM.unionWith mergeValues hm1 hm2
mergeValues _ v2 = v2 -- everything else just overwrites

updateConfig :: Config -> Value -> Either String Config
updateConfig cfg upd = case fromJSON $ mergeValues (toJSON cfg) upd of
  Error s      -> Left s
  Success cfg' -> Right cfg'

loadConfig :: FilePath -> IO Config
loadConfig fp = do
  valueOrError <- eitherDecode <$> BL.readFile fp
  case valueOrError of
    Left msg -> throwIO $ ConfigJSONError msg
    Right value -> case updateConfig defaultConfig value of
      Left msg  -> throwIO $ ConfigJSONError msg
      Right cfg -> return cfg

data ConfigJSONError = ConfigJSONError String deriving (Show)

instance Exception ConfigJSONError

saveConfig :: FilePath -> Config -> IO ()
saveConfig fp = BL.writeFile fp . encode
