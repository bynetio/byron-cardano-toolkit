{-# LANGUAGE OverloadedStrings #-}

module Cardano.Transaction where

import Cardano.Node.Cli (NodeCliConfig (..), cli, cliDefaultConfig, testnetMagic, touchFile)
import Control.Exception
  ( Exception,
    IOException,
    catch,
    throw,
    throwIO,
  )
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Reader
  ( Reader,
    ReaderT,
    reader,
    runReader,
  )
import Data.Aeson
import Data.String.Conversions
import Data.Typeable (Typeable)
import Data.UUID (UUID, fromString)
import System.Directory
import System.IO.Temp
import System.Random (randomIO)
import Wallet.Api (getWalletDir)
import Control.Applicative (liftA2)
import Data.Maybe (fromMaybe)

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

data TransactionException = TransactionDecodingFailure {tx :: String} deriving (Show, Typeable)

instance Exception TransactionException

signTx :: UUID -> CardanoTransaction -> ReaderT NodeCliConfig IO CardanoTransaction
signTx uuid tx = do
  cfg <- ask
  walletDir <- fromReader $ getWalletDir uuid
  let signKey = walletDir <> "/" <> "payment.skey"
  liftIO $
    withSystemTempDirectory "sign-tx" $ \tmpDir -> do
      copyFile signKey $ tmpDir <> "/payment.skey"
      writeFile (tmpDir <> "/tx.draft") $ convertString (encode tx)
      s <- sign cfg {nlcOutDir = tmpDir}
      case decode (convertString s) of
        Just tx -> return tx
        Nothing -> throw $ TransactionDecodingFailure (convertString s)
  where
    sign :: NodeCliConfig -> IO String
    sign cfg =
      cli
        cfg
        $ [ "transaction",
            "sign",
            "--tx-body-file",
            "/out/tx.draft",
            "--signing-key-file",
            "/out/payment.skey",
            "--out-file",
            "/dev/stdout"
          ]
          <> testnetMagic cfg