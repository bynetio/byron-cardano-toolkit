{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Node.Cli where

import Control.Exception (Exception, IOException, catch, throw)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader
  ( Reader,
    ReaderT,
    ask,
    reader,
    runReader,
  )
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (maybeToList)
import GHC.Generics (Generic)
import System.Process (readProcess)

data NodeCliConfig = NodeCliConfig
  { nlcOutDir :: String,
    nlcNetwork :: String,
    nlcContainerName :: String,
    nlcDockerImage :: String,
    nlcTestnetMagic :: Maybe String
  }
  deriving (Eq, Show, Generic)

cliDefaultConfig :: NodeCliConfig
cliDefaultConfig =
  NodeCliConfig
    { nlcOutDir = "/home/ssledz/now/out",
      nlcNetwork = "testnet",
      nlcContainerName = "node-cli",
      nlcDockerImage = "inputoutput/cardano-node:1.29.0-rc2",
      nlcTestnetMagic = Just "8"
    }

data Tip = Tip
  { epoch :: Integer,
    hash :: String,
    slot :: Integer,
    block :: Integer,
    era :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Tip

instance FromJSON Tip

tip :: ReaderT NodeCliConfig IO String
tip = do
  cfg <- ask
  liftIO $ cli cfg ["query", "tip", "--testnet-magic", "8"]

fromReader :: Monad m => Reader r a -> ReaderT r m a
fromReader = reader . runReader

touchFile :: FilePath -> IO ()
touchFile p = writeFile p ""

testnetMagic :: NodeCliConfig -> [String]
testnetMagic cfg = maybeToList (nlcTestnetMagic cfg) >>= \m -> ["--testnet-magic", m]

cli :: NodeCliConfig -> [String] -> IO String
cli cfg args = readProcess "docker" (args' <> args) ""
  where
    args' =
      [ "run",
        "--name",
        nlcContainerName cfg,
        "--rm",
        "--entrypoint",
        "cardano-cli",
        "-e",
        "NETWORK=" <> nlcNetwork cfg,
        "-e",
        "CARDANO_NODE_SOCKET_PATH=/ipc/socket",
        "-v",
        "node-ipc:/ipc",
        "-v",
        nlcOutDir cfg <> ":/out",
        nlcDockerImage cfg
      ]
