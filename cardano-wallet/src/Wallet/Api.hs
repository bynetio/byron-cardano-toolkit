{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Wallet.Api where

import Cardano.Node.Cli (NodeCliConfig (..), NodeCliException (..), cli, testnetMagic, touchFile)
import Control.Exception
  ( Exception,
    IOException,
    catch,
    throw,
  )
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader
  ( Reader,
    ReaderT,
    ask,
    reader,
    runReader,
  )
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (catMaybes)
import Data.UUID (UUID, toString)
import GHC.Generics (Generic)
import Safe (headMay)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
  )
import System.Random (randomIO)

data CreateWalletParam = CreateWalletParam {cwpName :: String, cwpDesc :: Maybe String} deriving (Eq, Show, Generic)

data Wallet = Wallet
  { identifier :: String,
    name :: String,
    desc :: Maybe String,
    address :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON CreateWalletParam

instance FromJSON CreateWalletParam

instance ToJSON Wallet

instance FromJSON Wallet

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