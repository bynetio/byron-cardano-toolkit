{-# LANGUAGE OverloadedStrings #-}

module Cardano.Transaction where

import Cardano.Node.Cli (NodeCliConfig (..), NodeCliException (..), cli, testnetMagic, touchFile)
import Control.Exception
  ( Exception,
    IOException,
    catch,
    throw,
  )
import Control.Monad (void)
import Control.Monad.Trans.Reader
  ( Reader,
    ReaderT,
    reader,
    runReader,
  )
import Data.Aeson
import Data.UUID (UUID)
import Wallet.Api (getWalletDir)

fromReader :: Monad m => Reader r a -> ReaderT r m a
fromReader = reader . runReader

data CardanoTransaction = CardanoTransaction
  { ctType :: String,
    ctDescription :: String,
    ctCborHex :: String
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