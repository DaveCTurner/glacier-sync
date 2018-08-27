module CliConfig where

import           Data.Monoid
import           Options.Applicative

data CliConfig = CliConfig
  { cliConfigDataPath :: FilePath
  }

cliConfig :: Parser CliConfig
cliConfig = CliConfig
  <$> strOption
    (long "data-path"
      <> metavar "PATH"
      <> help "Path to data directory (contains the mirror subdirectory and the database)")

parseCliConfig :: IO CliConfig
parseCliConfig = execParser $ info (cliConfig <**> helper)
      ( fullDesc
     <> progDesc "Server for sync with AWS Glacier"
     <> header "glacier-sync - sync with AWS Glacier")
