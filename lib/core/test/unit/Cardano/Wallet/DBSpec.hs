{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DBSpec
    ( spec
    , KeyValPairs (..)
    , DummyTarget
    , DummyStateMVar (..)
    , DummyStateSqlite (..)
    , once
    , once_
    , lrp
    , unions
    , prop_createListWallet
    , prop_createWalletTwice
    , prop_removeWalletTwice
    , prop_readAfterPut
    , prop_putBeforeInit
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( unXPrv )
import Cardano.Wallet
    ( unsafeRunExceptT )
import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Key, Passphrase (..), XPrv, generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet, initWallet )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Direction (..)
    , Hash (..)
    , SlotId (..)
    , Tx (..)
    , TxId (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxStatus (..)
    , UTxO (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( forM, void )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Crypto.Hash
    ( hash )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )
import System.IO.Unsafe
    ( unsafePerformIO )
import Test.Hspec
    ( Spec, shouldBe, shouldReturn )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , Property
    , arbitraryBoundedEnum
    , choose
    , elements
    , generate
    , genericShrink
    , oneof
    , scale
    , vectorOf
    )
import Test.QuickCheck.Instances.Time
    ()
import Test.QuickCheck.Monadic
    ( monadicIO, pick )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as Map

spec :: Spec
spec = return ()

{-------------------------------------------------------------------------------
                    Cross DB Specs Shared Arbitrary Instances
-------------------------------------------------------------------------------}
-- | Keep only the (L)ast (R)ecently (P)ut entry
lrp :: (Applicative f, Ord k) => [(k, v)] -> [f v]
lrp = Map.elems . foldl (\m (k, v) -> Map.insert k (pure v) m) mempty

-- | Keep the unions (right-biaised) of all entry
unions :: (Monoid v, Ord k) => [(k, v)] -> [Identity v]
unions =
    fmap Identity
    . Map.elems
    . foldl (\m (k, v) -> Map.unionWith (<>) (Map.fromList [(k, v)]) m) mempty

-- | Execute an action once per key @k@ present in the given list
once :: (Ord k, Monad m) => [(k,v)] -> ((k,v) -> m a) -> m [a]
once xs = forM (Map.toList (Map.fromList xs))

-- | Like 'once', but discards the result
once_ :: (Ord k, Monad m) => [(k,v)] -> ((k,v) -> m a) -> m ()
once_ xs = void . once xs


newtype KeyValPairs k v = KeyValPairs [(k, v)]
    deriving (Generic, Show, Eq)

instance (Arbitrary k, Arbitrary v) => Arbitrary (KeyValPairs k v) where
    shrink = genericShrink
    arbitrary = do
        pairs <- choose (10, 50) >>= flip vectorOf arbitrary
        pure $ KeyValPairs pairs

data DummyTarget

instance TxId DummyTarget where
    txId = Hash . B8.pack . show

newtype DummyStateMVar = DummyStateMVar Int
    deriving (Show, Eq)

instance Arbitrary DummyStateMVar where
    shrink _ = []
    arbitrary = DummyStateMVar <$> arbitrary

deriving instance NFData DummyStateMVar

instance IsOurs DummyStateMVar where
    isOurs _ num = (True, num)

instance Arbitrary (Wallet DummyStateMVar DummyTarget) where
    shrink _ = []
    arbitrary = initWallet <$> arbitrary

-- wallet state is UTxO and Pending transactions
newtype PendingTx = PendingTx [(TxIn, TxOut)]
    deriving (Show, Eq, NFData)

instance Arbitrary PendingTx where
    shrink _ = []
    arbitrary = do
        k <- choose (0, 6)
        pending <- zip
            <$> vectorOf k arbitrary
            <*> vectorOf k arbitrary
        return $ PendingTx pending

newtype DummyStateSqlite = DummyStateSqlite (UTxO, PendingTx)
    deriving (Show, Eq, NFData)

instance Arbitrary DummyStateSqlite where
    shrink _ = []
    arbitrary = DummyStateSqlite <$> arbitrary

instance IsOurs DummyStateSqlite where
    isOurs _ wm = (True, wm)

instance Arbitrary (Wallet DummyStateSqlite DummyTarget) where
    shrink _ = []
    arbitrary = initWallet <$> arbitrary

instance Arbitrary (PrimaryKey WalletId) where
    shrink _ = []
    arbitrary = do
        bytes <- B8.pack . pure <$> elements ['a'..'k']
        return $ PrimaryKey $ WalletId $ hash bytes

deriving instance Show (PrimaryKey WalletId)

instance Arbitrary (Hash "Tx") where
    shrink _ = []
    arbitrary = do
        k <- choose (0, 10)
        return $ Hash (B8.pack ("TX" <> show @Int k))

instance Arbitrary Tx where
    shrink _ = []
    arbitrary = return $ Tx mempty mempty

instance Arbitrary TxMeta where
    shrink _ = []
    arbitrary = TxMeta
        <$> elements [Pending, InLedger, Invalidated]
        <*> elements [Incoming, Outgoing]
        <*> (SlotId <$> arbitrary <*> choose (0, 21600))
        <*> fmap (Quantity . fromIntegral) (arbitrary @Word32)

instance Arbitrary WalletMetadata where
    shrink _ = []
    arbitrary = WalletMetadata
        <$> (WalletName <$> elements ["bulbazaur", "charmander", "squirtle"])
        <*> (fmap WalletPassphraseInfo <$> arbitrary)
        <*> oneof [pure Ready, Restoring . Quantity <$> arbitraryBoundedEnum]
        <*> pure NotDelegating

instance Arbitrary Address where
    -- No Shrinking
    arbitrary = oneof
        [ pure $ Address "ADDR01"
        , pure $ Address "ADDR02"
        , pure $ Address "ADDR03"
        ]

instance Arbitrary Coin where
    -- No Shrinking
    arbitrary = Coin <$> choose (1, 100000)

instance Arbitrary TxIn where
    -- No Shrinking
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a high indexes

instance Arbitrary TxOut where
    -- No Shrinking
    arbitrary = TxOut
        <$> arbitrary
        <*> arbitrary

instance Arbitrary UTxO where
    shrink (UTxO utxo) = UTxO <$> shrink utxo
    arbitrary = do
        n <- choose (1, 100)
        utxo <- zip
            <$> vectorOf n arbitrary
            <*> vectorOf n arbitrary
        return $ UTxO $ Map.fromList utxo

instance Arbitrary (Key 'RootK XPrv) where
    shrink _ = []
    arbitrary = elements rootKeys

instance Arbitrary (Hash "encryption") where
    shrink _ = []
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        return $ Hash $ BS.pack $ take 32 bytes

-- Necessary unsound Show instance for QuickCheck failure reporting
instance Show XPrv where
    show = show . unXPrv

-- Necessary unsound Eq instance for QuickCheck properties
instance Eq XPrv where
    a == b = unXPrv a == unXPrv b

genRootKeys :: Gen (Key 'RootK XPrv)
genRootKeys = do
    (s, g, e) <- (,,)
        <$> genPassphrase @"seed" (0, 32)
        <*> genPassphrase @"generation" (0, 16)
        <*> genPassphrase @"encryption" (0, 16)
    return $ generateKeyFromSeed (s, g) e
  where
    genPassphrase :: (Int, Int) -> Gen (Passphrase purpose)
    genPassphrase range = do
        n <- choose range
        InfiniteList bytes _ <- arbitrary
        return $ Passphrase $ BA.convert $ BS.pack $ take n bytes

-- Properties above are quite heavy on the generation of values, althrough for
-- private keys, it isn't particularly useful / relevant to generate many of
-- them as they're really treated as an opaque type.
-- Instead, we generate them once, and picks from the list.
rootKeys :: [Key 'RootK XPrv]
rootKeys = unsafePerformIO $ generate (vectorOf 10 genRootKeys)
{-# NOINLINE rootKeys #-}


{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}

-- | Can list created wallets
prop_createListWallet
    :: (Show s, Eq s, IsOurs s, NFData s)
    => DBLayer IO s DummyTarget
    -> KeyValPairs (PrimaryKey WalletId) (Wallet s DummyTarget, WalletMetadata)
    -> Property
prop_createListWallet dbLayer (KeyValPairs pairs) =
    monadicIO (pure dbLayer >>= prop)
  where
    prop db = liftIO $ do
        res <- once pairs $ \(k, (cp, meta)) ->
            unsafeRunExceptT $ createWallet db k cp meta
        (length <$> listWallets db) `shouldReturn` length res

-- | Trying to create a same wallet twice should yield an error
prop_createWalletTwice
    :: (Show s, Eq s, IsOurs s, NFData s)
    => DBLayer IO s DummyTarget
    -> ( PrimaryKey WalletId
       , Wallet s DummyTarget
       , WalletMetadata
       )
    -> Property
prop_createWalletTwice dbLayer (key@(PrimaryKey wid), cp, meta) =
    monadicIO (pure dbLayer >>= prop)
  where
    prop db = liftIO $ do
        let err = ErrWalletAlreadyExists wid
        runExceptT (createWallet db key cp meta) `shouldReturn` Right ()
        runExceptT (createWallet db key cp meta) `shouldReturn` Left err

-- | Trying to remove a same wallet twice should yield an error
prop_removeWalletTwice
    :: (Show s, Eq s, IsOurs s, NFData s)
    => DBLayer IO s DummyTarget
    -> ( PrimaryKey WalletId
       , Wallet s DummyTarget
       , WalletMetadata
       )
    -> Property
prop_removeWalletTwice dbLayer (key@(PrimaryKey wid), cp, meta) =
    monadicIO (setup >>= prop)
  where
    setup = liftIO $ do
        unsafeRunExceptT $ createWallet dbLayer key cp meta
        return dbLayer
    prop db = liftIO $ do
        let err = ErrNoSuchWallet wid
        runExceptT (removeWallet db key) `shouldReturn` Right ()
        runExceptT (removeWallet db key) `shouldReturn` Left err

-- | Checks that a given resource can be read after having been inserted in DB.
prop_readAfterPut
    :: (Show (f a), Eq (f a), Applicative f, Show s, Eq s, IsOurs s, NFData s, Arbitrary (Wallet s DummyTarget))
    => (  DBLayer IO s DummyTarget
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s DummyTarget
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> DBLayer IO s DummyTarget
    -> (PrimaryKey WalletId, a)
        -- ^ Property arguments
    -> Property
prop_readAfterPut putOp readOp dbLayer (key, a) =
    monadicIO (setup >>= prop)
  where
    setup = do
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet dbLayer key cp meta
        return dbLayer
    prop db = liftIO $ do
        unsafeRunExceptT $ putOp db key a
        res <- readOp db key
        res `shouldBe` pure a

-- | Can't put resource before a wallet has been initialized
prop_putBeforeInit
    :: (Show (f a), Eq (f a), Applicative f, Show s, Eq s, IsOurs s, NFData s)
    => (  DBLayer IO s DummyTarget
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO s DummyTarget
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> f a
        -- ^ An 'empty' value for the 'Applicative' f
    -> DBLayer IO s DummyTarget
    -> (PrimaryKey WalletId, a)
        -- ^ Property arguments
    -> Property
prop_putBeforeInit putOp readOp empty dbLayer (key@(PrimaryKey wid), a) =
    monadicIO (pure dbLayer >>= prop)
  where
    prop db = liftIO $ do
        runExceptT (putOp db key a) >>= \case
            Right _ ->
                fail "expected put operation to fail but it succeeded!"
            Left err ->
                err `shouldBe` ErrNoSuchWallet wid
        readOp db key `shouldReturn` empty
