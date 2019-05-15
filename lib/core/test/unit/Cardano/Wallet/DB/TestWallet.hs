{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.TestWallet where

import Prelude

import Control.DeepSeq
    ( NFData )
import GHC.Generics
    ( Generic )
import Test.QuickCheck
    ( Arbitrary (..) )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet, initWallet )
import Cardano.Wallet.Primitive.Types
    ( Hash (..)
    , TxId (..)
    , WalletDelegation (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletState (..)
    )


{-------------------------------------------------------------------------------
                      Tests machinery, Arbitrary instances
                               fixme: copy&pasted
-------------------------------------------------------------------------------}

data DummyTarget

instance TxId DummyTarget where
    txId = Hash . B8.pack . show

newtype DummyState = DummyState Int
    deriving (Show, Eq, Generic)

instance Arbitrary DummyState where
    shrink _ = []
    arbitrary = DummyState <$> arbitrary

deriving instance NFData DummyState

instance IsOurs DummyState where
    isOurs _ num = (True, num)

instance Arbitrary (Wallet DummyState DummyTarget) where
    shrink _ = []
    arbitrary = initWallet <$> arbitrary

instance Arbitrary WalletMetadata where
    shrink _ = []
    arbitrary = WalletMetadata
        <$> (fmap (WalletName . T.pack) arbitrary)
        <*> pure Nothing
        <*> pure Ready
        <*> pure NotDelegating
