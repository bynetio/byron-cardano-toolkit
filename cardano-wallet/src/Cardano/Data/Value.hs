{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Data.Value (tokenName, currencySymbol, CurrencySymbol, TokenName) where

import qualified Data.ByteString as BS
import Data.Map as Map ( lookup, Map, keys )
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

newtype CurrencySymbol = CurrencySymbol {unCurrencySymbol :: BS.ByteString} deriving (Eq, Show, Generic, Ord)

currencySymbol :: BS.ByteString -> CurrencySymbol
currencySymbol = CurrencySymbol

newtype TokenName = TokenName {unTokenName :: BS.ByteString} deriving (Eq, Show, Generic, Ord)

tokenName :: BS.ByteString -> TokenName
tokenName = TokenName

newtype AssetClass = AssetClass {unAssetClass :: (CurrencySymbol, TokenName)} deriving (Eq, Show, Generic)

assetClass :: CurrencySymbol -> TokenName -> AssetClass
assetClass c t = AssetClass (c, t)

newtype Value = Value {getValue :: Map.Map CurrencySymbol (Map.Map TokenName Integer)} deriving (Eq, Show, Generic)

valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
valueOf (Value mp) cur tn =
  case Map.lookup cur mp of
    Nothing -> 0
    Just i -> fromMaybe 0 (Map.lookup tn i)

symbols :: Value -> [CurrencySymbol]
symbols (Value mp) = Map.keys mp