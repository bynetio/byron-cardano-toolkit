{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module NodeCli where

import           Control.Exception          (Exception, IOException, catch,
                                             throw)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy       as BSL
import Data.String.Conversions
import           Data.Maybe
import           Data.Typeable
import           Data.UUID
import           GHC.Generics
import           Network.HTTP.Media         ((//), (/:))
import           Safe
import           Servant
import           System.Directory
import           System.Process
import           System.Random
import Network.Wai.Handler.Warp ( run )

data NodeCliConfig = NodeCliConfig
  { nlcOutDir        :: String,
    nlcNetwork       :: String,
    nlcContainerName :: String,
    nlcDockerImage   :: String,
    nlcTestnetMagic  :: Maybe String
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

tip :: ReaderT NodeCliConfig IO String
tip = do
  cfg <- ask
  liftIO $ cli cfg ["query", "tip", "--testnet-magic", "8"]

data NodeCliException = CreateWalletException !String deriving (Show, Typeable)

instance Exception NodeCliException

fromReader :: Monad m => Reader r a -> ReaderT r m a
fromReader = reader . runReader

getWalletDirStore :: Reader NodeCliConfig String
getWalletDirStore = do
  cfg <- ask
  return $ nlcOutDir cfg <> "/wallets"

getWalletDir :: UUID -> Reader NodeCliConfig String
getWalletDir uuid = do
  walletStore <- getWalletDirStore
  return $ walletStore <> "/" <> toString uuid

touchFile :: FilePath -> IO ()
touchFile p = writeFile p ""

data CreateWalletParam = CreateWalletParam {cwpName :: String, cwpDesc :: Maybe String} deriving (Eq, Show, Generic)

data Wallet = Wallet
  { identifier :: String,
    name       :: String,
    desc       :: Maybe String,
    address    :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON CreateWalletParam

instance FromJSON CreateWalletParam

instance ToJSON Wallet

instance FromJSON Wallet

testnetMagic :: NodeCliConfig -> [String]
testnetMagic cfg = maybeToList (nlcTestnetMagic cfg) >>= \m -> ["--testnet-magic", m]

createWallet :: CreateWalletParam -> ReaderT NodeCliConfig IO Wallet
createWallet cw = do
  cfg <- ask
  walletStore <- fromReader getWalletDirStore
  liftIO $ putStrLn $ "walletStore: " <> walletStore
  walletId <- liftIO genWalletId
  let walletDir = walletStore <> "/" <> toString walletId
  dirExists <- liftIO $ doesDirectoryExist walletDir
  when dirExists $ throw (CreateWalletException $ "wallet " <> toString walletId <> " already exists")
  liftIO $ createDirectoryIfMissing True walletDir
  liftIO $ touchFiles walletDir
  let cfg' = cfg {nlcOutDir = walletDir}
  void $ liftIO $ sequenceA (keyGen cfg' <$> [("address", "payment"), ("stake-address", "stake")])
  liftIO $ addressGen cfg'
  walletAddress <- liftIO $ getAddress walletDir
  let walletMeta = Wallet {identifier = toString walletId, name = cwpName cw, desc = cwpDesc cw, address = walletAddress}
  liftIO $ BSL.writeFile (walletDir <> "/meta.json") $ encode walletMeta
  return walletMeta
  where
    genWalletId :: IO UUID
    genWalletId = randomIO

    getAddress :: FilePath -> IO String
    getAddress dir = readFile (dir <> "/" <> "wallet.addr")

    touchFiles :: FilePath -> IO ()
    touchFiles walletDir = do
      let files = (\n -> walletDir <> "/" <> n) <$> ["payment.skey", "payment.vkey", "stake.skey", "stake.vkey", "wallet.addr"]
      sequence_ (touchFile <$> files)

    keyGen :: NodeCliConfig -> (String, String) -> IO ()
    keyGen cfg (addressType, keyName) =
      void $
        cli
          cfg
          [ addressType,
            "key-gen",
            "--verification-key-file",
            "/out/" <> keyName <> ".vkey",
            "--signing-key-file",
            "/out/" <> keyName <> ".skey"
          ]

    addressGen :: NodeCliConfig -> IO ()
    addressGen cfg =
      let args =
            [ "address",
              "build",
              "--payment-verification-key-file",
              "/out/payment.vkey",
              "--stake-verification-key-file",
              "/out/stake.vkey",
              "--out-file",
              "/out/wallet.addr"
            ]
              <> testnetMagic cfg
       in void $ cli cfg args

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

signTx :: UUID -> CardanoTransaction -> ReaderT NodeCliConfig IO CardanoTransaction
signTx uuid tx = do
  walletDir <- fromReader $ getWalletDir uuid
  let signKey = walletDir <> "/" <> "payment.skey"
  return tx
  where
    sign :: NodeCliConfig -> IO ()
    sign cfg =
      void $
        cli
          cfg
          $ [ "transaction",
              "sign",
              "--tx-body-file",
              "/out/tx.draft",
              "--signing-key-file",
              "/out/payment.skey",
              "--testnet-magic $TESTNET_MAGIC",
              "--out-file",
              "/out/tx.signed"
            ]
            <> testnetMagic cfg

listWallets :: ReaderT NodeCliConfig IO [Wallet]
listWallets = do
  walletStore <- fromReader getWalletDirStore
  files <- liftIO $ map (filePath walletStore) <$> listDirectory walletStore
  xs <- liftIO $ sequenceA (BSL.readFile <$> files)
  return $ catMaybes (parseWallet <$> xs)
  where
    filePath :: FilePath -> FilePath -> FilePath
    filePath root dir = root <> "/" <> dir <> "/meta.json"

    -- parseWallet :: BLU.ByteString -> Maybe Wallet
    parseWallet :: BSL.ByteString -> Maybe Wallet
    parseWallet str = decode str

getWalletById :: UUID -> ReaderT NodeCliConfig IO (Maybe Wallet)
getWalletById uuid = headMay . filter (\w -> toString uuid == identifier w) <$> listWallets

getWalletKey :: UUID -> String -> ReaderT NodeCliConfig IO (Maybe String)
getWalletKey uuid keyFileName = do
  walletStore <- fromReader getWalletDirStore
  let filePath = walletStore <> "/" <> toString uuid <> "/" <> keyFileName
  liftIO $
    catch (Just <$> readFile filePath) $ \e -> do
      putStrLn $ "Exception during getWalletKey: " <> show (e :: IOException)
      return Nothing

data CardanoTransaction = CardanoTransaction
  { ctType        :: String,
    ctDescription :: String,
    ctCborHex     :: String
  }
  deriving (Eq, Show)

instance ToJSON CardanoTransaction where
  toJSON tx =
    object
      [ "type" .= ctType tx,
        "description" .= ctDescription tx,
        "cborHex" .= ctCborHex tx
      ]

instance FromJSON CardanoTransaction where
  parseJSON (Object x) = CardanoTransaction <$> x .: "type" <*> x .: "description" <*> x .: "cborHex"
  parseJSON _ = fail "Expected an Object"

data JsonRaw

type CliAPI =
  "tip" :> Get '[JSON] (Maybe Tip)
    :<|> "wallet" :> ReqBody '[JSON] CreateWalletParam :> Put '[JSON] Wallet
    :<|> "wallet" :> Get '[JSON] [Wallet]
    :<|> "wallet" :> Capture "id" UUID :> Get '[JSON] (Maybe Wallet)
    :<|> "wallet" :> Capture "id" UUID :> "vkey" :> Get '[JsonRaw] (Maybe String)
    :<|> "wallet" :> Capture "id" UUID :> "signTx" :> ReqBody '[JSON] CardanoTransaction :> Post '[JSON] CardanoTransaction

instance Accept JsonRaw where
  contentType _ = "application" // "json" /: ("charset", "utf-8")

instance MimeRender JsonRaw (Maybe String) where
  mimeRender _ (Just s) = convertString s
  mimeRender _ Nothing  = ""

data Tip = Tip
  { epoch :: Integer,
    hash  :: String,
    slot  :: Integer,
    block :: Integer,
    era   :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Tip

instance FromJSON Tip

cliServer :: Server CliAPI
cliServer = handleTip :<|> handleCreateWallet :<|> handleListWallets :<|> handleGetWallet :<|> handleGetPubKey :<|> handleSignTx
  where
    handleSignTx :: UUID -> CardanoTransaction -> Handler CardanoTransaction
    handleSignTx uuid tx = liftIO $ runReaderT (signTx uuid tx) cliDefaultConfig

    handleGetPubKey :: UUID -> Handler (Maybe String)
    handleGetPubKey uuid = liftIO $ runReaderT (getWalletKey uuid "payment.vkey") cliDefaultConfig

    handleGetWallet :: UUID -> Handler (Maybe Wallet)
    handleGetWallet uuid = liftIO $ runReaderT (getWalletById uuid) cliDefaultConfig

    handleListWallets :: Handler [Wallet]
    handleListWallets = liftIO $ runReaderT listWallets cliDefaultConfig

    handleCreateWallet :: CreateWalletParam -> Handler Wallet
    handleCreateWallet cwp = liftIO $ runReaderT (createWallet cwp) cliDefaultConfig

    handleTip :: Handler (Maybe Tip)
    handleTip = do
      json <- liftIO $ runTip
      let parsed = decode (convertString json) :: Maybe Tip
      liftIO $ putStrLn json
      liftIO $ putStrLn (show parsed)
      return parsed
    runTip = runReaderT tip cliDefaultConfig

cliAPI :: Proxy CliAPI
cliAPI = Proxy

cliApp :: Application
cliApp = serve cliAPI cliServer

runApp :: Application -> Int -> IO ()
runApp app port = run port app
