{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.SqliteSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet
    ( unsafeRunExceptT )
import Cardano.Wallet.DB
    ( DBLayer (..), PrimaryKey (..) )
import Cardano.Wallet.DB.Sqlite
    ( newDBLayer )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.Model
    ( initWallet )
import Cardano.Wallet.Primitive.Types
    ( Hash (..)
    , TxId (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    )
import Control.DeepSeq
    ( NFData )
import Crypto.Hash
    ( hash )
import Data.ByteString
    ( ByteString )
import Data.Time.Clock
    ( getCurrentTime )
import Test.Hspec
    ( Spec, describe, it, shouldReturn )

import qualified Data.ByteString.Char8 as B8

spec :: Spec
spec = do
    describe "Wallet table" $ do
        it "create and list works" $ do
            db <- getDBLayer
            now <- getCurrentTime
            let wid = PrimaryKey (WalletId (hash ("test" :: ByteString)))
                md = WalletMetadata
                    { name = WalletName "test wallet"
                    , passphraseInfo = Just $ WalletPassphraseInfo now
                    , status = Ready
                    , delegation = NotDelegating
                    }
                cp = initWallet (DummyState md)
            unsafeRunExceptT (createWallet db wid cp md) `shouldReturn` ()
            listWallets db `shouldReturn` [wid]

        it "create and get meta works" $ do
            db <- getDBLayer
            now <- getCurrentTime
            let wid = PrimaryKey (WalletId (hash ("test" :: ByteString)))
                md = WalletMetadata
                    { name = WalletName "test wallet"
                    , passphraseInfo = Just $ WalletPassphraseInfo now
                    , status = Ready
                    , delegation = NotDelegating
                    }
                cp = initWallet (DummyState md)
            unsafeRunExceptT (createWallet db wid cp md) `shouldReturn` ()
            readWalletMeta db wid `shouldReturn` Just md
    where
        getDBLayer
            :: IO (DBLayer IO DummyState DummyTarget)
        getDBLayer = do newDBLayer Nothing

deriving instance Show (PrimaryKey WalletId)

data DummyTarget

instance TxId DummyTarget where
    txId = Hash . B8.pack . show

newtype DummyState = DummyState WalletMetadata
    deriving (Show, Eq)

deriving instance NFData DummyState

instance IsOurs DummyState where
    isOurs _ wm = (True, wm)
