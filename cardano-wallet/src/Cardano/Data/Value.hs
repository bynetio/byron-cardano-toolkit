{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Data.Value
  ( tokenName,
    currencySymbol,
    CurrencySymbol,
    TokenName,
    AssetClass,
    Value,
    assetClass,
    assetClassValue,
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Aeson as JSON
import Data.Map (Map, keys, lookup, unionWith)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Directory.Internal.Prelude (Semigroup)

newtype CurrencySymbol = CurrencySymbol {unCurrencySymbol :: T.Text}
  deriving (Eq, Show, Generic, Ord)
  deriving anyclass (ToJSONKey, FromJSONKey)

currencySymbol :: T.Text -> CurrencySymbol
currencySymbol = CurrencySymbol

newtype TokenName = TokenName {unTokenName :: T.Text}
  deriving (Eq, Show, Generic, Ord)
  deriving anyclass (ToJSONKey, FromJSONKey)

tokenName :: T.Text -> TokenName
tokenName = TokenName

newtype AssetClass = AssetClass {unAssetClass :: (CurrencySymbol, TokenName)} deriving (Eq, Show, Generic)

assetClass :: CurrencySymbol -> TokenName -> AssetClass
assetClass c t = AssetClass (c, t)

newtype Value = Value {getValue :: Map.Map CurrencySymbol (Map.Map TokenName Integer)} deriving (Eq, Show, Generic)

singleton :: CurrencySymbol -> TokenName -> Integer -> Value
singleton c tn i = Value (Map.singleton c (Map.singleton tn i))

assetClassValue :: AssetClass -> Integer -> Value
assetClassValue (AssetClass (c, t)) = singleton c t

valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
valueOf (Value mp) cur tn =
  case Map.lookup cur mp of
    Nothing -> 0
    Just i -> fromMaybe 0 (Map.lookup tn i)

symbols :: Value -> [CurrencySymbol]
symbols (Value mp) = Map.keys mp

instance Semigroup Value where
  Value l <> Value r = Value $ Map.unionWith f l r
    where
      f a b = Map.unionWith (+) a b

instance ToJSON TokenName

instance FromJSON TokenName

instance ToJSON AssetClass

instance FromJSON AssetClass

instance ToJSON Value

instance FromJSON Value

instance ToJSON CurrencySymbol

instance FromJSON CurrencySymbol
