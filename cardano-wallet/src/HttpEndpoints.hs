{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module HttpEndpoints where

import Cardano.Node.Cli (cliDefaultConfig, tip)
import Cardano.Transaction (CardanoTransaction, signTx)
import Control.Exception
  ( Exception,
    IOException,
    catch,
    throw,
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.String.Conversions
  ( ConvertibleStrings (convertString),
  )
import Data.Typeable (Proxy (..))
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import Servant
import Wallet.Api
  ( CreateWalletParam,
    Wallet,
    createWallet,
    getWalletById,
    getWalletKey,
    listWallets,
  )

data JsonRaw

type HttpAPI =
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
  mimeRender _ Nothing = ""

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

server :: Server HttpAPI
server = handleTip :<|> handleCreateWallet :<|> handleListWallets :<|> handleGetWallet :<|> handleGetPubKey :<|> handleSignTx
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
      json <- liftIO $ runReaderT tip cliDefaultConfig
      let parsed = decode (convertString json) :: Maybe Tip
      return parsed

proxyAPI :: Proxy HttpAPI
proxyAPI = Proxy

app :: Application
app = serve proxyAPI server