{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.SqliteSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet
    ( unsafeRunExceptT )
import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    )
import Cardano.Wallet.DB.Sqlite
    ( newDBLayer )
import Cardano.Wallet.DBSpec
    ( KeyValPairs (..), lrp, once, once_, unions )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet, initWallet )
import Cardano.Wallet.Primitive.Types
    ( Hash (..)
    , Tx (..)
    , TxId (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , UTxO (..)
    , WalletId (..)
    , WalletMetadata (..)
    )
import Control.Concurrent.Async
    ( forConcurrently_ )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Map.Strict
    ( Map )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldReturn, xit )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , checkCoverage
    , choose
    , cover
    , property
    , vectorOf
    )
import Test.QuickCheck.Monadic
    ( monadicIO, pick )

import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L

spec :: Spec
spec = do
    describe "Extra Properties about DB initialization" $ do
        it "createWallet . listWallets yields expected results"
            (property prop_createListWallet)
        xit "creating same wallet twice yields an error"
            (property prop_createWalletTwice)
        it "removing the same wallet twice yields an error"
            (property prop_removeWalletTwice)

    describe "put . read yields a result" $ do
        xit "Checkpoint"
            (property $ prop_readAfterPut putCheckpoint readCheckpoint)
        xit "Wallet Metadata"
            (property $ prop_readAfterPut putWalletMeta readWalletMeta)
        xit "Tx History"
            (property $ prop_readAfterPut putTxHistory readTxHistoryF)
        it "Private Key"
            (property $ prop_readAfterPut putPrivateKey readPrivateKey)

    describe "can't put before wallet exists" $ do
        xit "Checkpoint"
            (property $ prop_putBeforeInit putCheckpoint readCheckpoint Nothing)
        it "Wallet Metadata"
            (property $ prop_putBeforeInit putWalletMeta readWalletMeta Nothing)
        it "Tx History"
            (property $ prop_putBeforeInit putTxHistory readTxHistoryF (pure mempty))
        it "Private Key"
            (property $ prop_putBeforeInit putPrivateKey readPrivateKey Nothing)

    describe "put doesn't affect other resources" $ do
        xit "Checkpoint vs Wallet Metadata & Tx History & Private Key"
            (property $ prop_isolation putCheckpoint
                readWalletMeta
                readTxHistoryF
                readPrivateKey
            )
        xit "Wallet Metadata vs Tx History & Checkpoint & Private Key"
            (property $ prop_isolation putWalletMeta
                readTxHistoryF
                readCheckpoint
                readPrivateKey
            )
        xit "Tx History vs Checkpoint & Wallet Metadata & Private Key"
            (property $ prop_isolation putTxHistory
                readCheckpoint
                readWalletMeta
                readPrivateKey
            )
    describe "can't read after delete" $ do
        xit "Checkpoint"
            (property $ prop_readAfterDelete readCheckpoint Nothing)
        it "Wallet Metadata"
            (property $ prop_readAfterDelete readWalletMeta Nothing)
        it "Tx History"
            (property $ prop_readAfterDelete readTxHistoryF (pure mempty))
        it "Private Key"
            (property $ prop_readAfterDelete readPrivateKey Nothing)

    describe "sequential puts replace values in order" $ do
        xit "Checkpoint"
            (checkCoverage $ prop_sequentialPut putCheckpoint readCheckpoint lrp)
        xit "Wallet Metadata"
            (checkCoverage $ prop_sequentialPut putWalletMeta readWalletMeta lrp)
        xit "Tx History"
            (checkCoverage $ prop_sequentialPut putTxHistory readTxHistoryF unions)
        it "Private Key"
            (checkCoverage $ prop_sequentialPut putPrivateKey readPrivateKey lrp)

    describe "parallel puts replace values in _any_ order" $ do
        xit "Checkpoint"
            (checkCoverage $ prop_parallelPut putCheckpoint readCheckpoint
                (length . lrp @Maybe))
        xit "Wallet Metadata"
            (checkCoverage $ prop_parallelPut putWalletMeta readWalletMeta
                (length . lrp @Maybe))
        xit "Tx History"
            (checkCoverage $ prop_parallelPut putTxHistory readTxHistoryF
                (length . unions @(Map (Hash "Tx") (Tx, TxMeta))))
        xit "Private Key"
            (checkCoverage $ prop_parallelPut putPrivateKey readPrivateKey
                (length . lrp @Maybe))

    where
    -- | Wrap the result of 'readTxHistory' in an arbitrary identity Applicative
    readTxHistoryF
        :: (Monad m, IsOurs s, NFData s, Show s, TxId t)
        => DBLayer m s t
        -> PrimaryKey WalletId
        -> m (Identity (Map (Hash "Tx") (Tx, TxMeta)))
    readTxHistoryF db = fmap Identity . readTxHistory db


---- FROM THIS CODE CAN PROBABLY BE SHARED WITH MVARSPEC and put to DBSpec

-- | Can list created wallets
prop_createListWallet
    :: KeyValPairs (PrimaryKey WalletId) (Wallet DummyState DummyTarget, WalletMetadata)
    -> Property
prop_createListWallet (KeyValPairs pairs) =
    monadicIO (setup >>= prop)
  where
    setup = liftIO $ newDBLayer Nothing
    prop db = liftIO $ do
        res <- once pairs $ \(k, (cp, meta)) ->
            unsafeRunExceptT $ createWallet db k cp meta
        (length <$> listWallets db) `shouldReturn` length res


-- | Trying to create a same wallet twice should yield an error
prop_createWalletTwice
    :: ( PrimaryKey WalletId
       , Wallet DummyState DummyTarget
       , WalletMetadata
       )
    -> Property
prop_createWalletTwice (key@(PrimaryKey wid), cp, meta) =
    monadicIO (setup >>= prop)
  where
    setup = liftIO $ newDBLayer Nothing
    prop db = liftIO $ do
        let err = ErrWalletAlreadyExists wid
        runExceptT (createWallet db key cp meta) `shouldReturn` Right ()
        runExceptT (createWallet db key cp meta) `shouldReturn` Left err

-- | Trying to remove a same wallet twice should yield an error
prop_removeWalletTwice
    :: ( PrimaryKey WalletId
       , Wallet DummyState DummyTarget
       , WalletMetadata
       )
    -> Property
prop_removeWalletTwice (key@(PrimaryKey wid), cp, meta) =
    monadicIO (setup >>= prop)
  where
    setup = liftIO $ do
        db <- newDBLayer Nothing
        unsafeRunExceptT $ createWallet db key cp meta
        return db
    prop db = liftIO $ do
        let err = ErrNoSuchWallet wid
        runExceptT (removeWallet db key) `shouldReturn` Right ()
        runExceptT (removeWallet db key) `shouldReturn` Left err

-- | Checks that a given resource can be read after having been inserted in DB.
prop_readAfterPut
    :: (Show (f a), Eq (f a), Applicative f)
    => (  DBLayer IO DummyState DummyTarget
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO DummyState DummyTarget
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> (PrimaryKey WalletId, a)
        -- ^ Property arguments
    -> Property
prop_readAfterPut putOp readOp (key, a) =
    monadicIO (setup >>= prop)
  where
    setup = do
        db <- liftIO $ newDBLayer Nothing
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet db key cp meta
        return db
    prop db = liftIO $ do
        unsafeRunExceptT $ putOp db key a
        res <- readOp db key
        res `shouldBe` pure a

-- | Can't put resource before a wallet has been initialized
prop_putBeforeInit
    :: (Show (f a), Eq (f a), Applicative f)
    => (  DBLayer IO DummyState DummyTarget
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO DummyState DummyTarget
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> f a
        -- ^ An 'empty' value for the 'Applicative' f
    -> (PrimaryKey WalletId, a)
        -- ^ Property arguments
    -> Property
prop_putBeforeInit putOp readOp empty (key@(PrimaryKey wid), a) =
    monadicIO (setup >>= prop)
  where
    setup = liftIO $ newDBLayer Nothing
    prop db = liftIO $ do
        runExceptT (putOp db key a) >>= \case
            Right _ ->
                fail "expected put operation to fail but it succeeded!"
            Left err ->
                err `shouldBe` ErrNoSuchWallet wid
        readOp db key `shouldReturn` empty

-- | Modifying one resource leaves the other untouched
prop_isolation
    :: ( Applicative f, Show (f b), Eq (f b)
       , Applicative g, Show (g c), Eq (g c)
       , Applicative h, Show (h d), Eq (h d)
       )
    => (  DBLayer IO DummyState DummyTarget
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO DummyState DummyTarget
       -> PrimaryKey WalletId
       -> IO (f b)
       ) -- ^ Read Operation for another resource
    -> (  DBLayer IO DummyState DummyTarget
       -> PrimaryKey WalletId
       -> IO (g c)
       ) -- ^ Read Operation for another resource
    -> (  DBLayer IO DummyState DummyTarget
       -> PrimaryKey WalletId
       -> IO (h d)
       ) -- ^ Read Operation for another resource
    -> (PrimaryKey WalletId, a)
        -- ^ Properties arguments
    -> Property
prop_isolation putA readB readC readD (key, a) =
    monadicIO (setup >>= prop)
  where
    setup = do
        db <- liftIO $ newDBLayer Nothing
        (cp, meta, txs) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet db key cp meta
        liftIO $ unsafeRunExceptT $ putTxHistory db key txs
        (b, c, d) <- liftIO $ (,,)
            <$> readB db key
            <*> readC db key
            <*> readD db key
        return (db, (b, c, d))

    prop (db, (b, c, d)) = liftIO $ do
        unsafeRunExceptT $ putA db key a
        readB db key `shouldReturn` b
        readC db key `shouldReturn` c
        readD db key `shouldReturn` d

-- | Can't read back data after delete
prop_readAfterDelete
    :: (Show (f a), Eq (f a), Applicative f)
    => (  DBLayer IO DummyState DummyTarget
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> f a
        -- ^ An 'empty' value for the 'Applicative' f
    -> PrimaryKey WalletId
    -> Property
prop_readAfterDelete readOp empty key =
    monadicIO (setup >>= prop)
  where
    setup = do
        db <- liftIO $ newDBLayer Nothing
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ createWallet db key cp meta
        return db
    prop db = liftIO $ do
        unsafeRunExceptT $ removeWallet db key
        readOp db key `shouldReturn` empty

-- | Check that the DB supports multiple sequential puts for a given resource
prop_sequentialPut
    :: (Show (f a), Eq (f a), Applicative f)
    => (  DBLayer IO DummyState DummyTarget
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO DummyState DummyTarget
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> (forall k. Ord k => [(k, a)] -> [f a])
        -- ^ How do we expect operations to resolve
    -> KeyValPairs (PrimaryKey WalletId) a
        -- ^ Property arguments
    -> Property
prop_sequentialPut putOp readOp resolve (KeyValPairs pairs) =
    cover 90 cond "conflicting db entries" $ monadicIO (setup >>= prop)
  where
    -- Make sure that we have some conflicting insertion to actually test the
    -- semantic of the DB Layer.
    cond = L.length (L.nub ids) /= L.length ids
      where
        ids = map fst pairs
    setup = do
        db <- liftIO $ newDBLayer Nothing
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ once_ pairs $ \(k, _) ->
            createWallet db k cp meta
        return db
    prop db = liftIO $ do
        unsafeRunExceptT $ forM_ pairs $ uncurry (putOp db)
        res <- once pairs (readOp db . fst)
        res `shouldBe` resolve pairs

-- | Check that the DB supports multiple sequential puts for a given resource
prop_parallelPut
    :: (Show (f a), Eq (f a), Applicative f)
    => (  DBLayer IO DummyState DummyTarget
       -> PrimaryKey WalletId
       -> a
       -> ExceptT ErrNoSuchWallet IO ()
       ) -- ^ Put Operation
    -> (  DBLayer IO DummyState DummyTarget
       -> PrimaryKey WalletId
       -> IO (f a)
       ) -- ^ Read Operation
    -> (forall k. Ord k => [(k, a)] -> Int)
        -- ^ How many entries to we expect in the end
    -> KeyValPairs (PrimaryKey WalletId) a
        -- ^ Property arguments
    -> Property
prop_parallelPut putOp readOp resolve (KeyValPairs pairs) =
    cover 90 cond "conflicting db entries" $ monadicIO (setup >>= prop)
  where
    -- Make sure that we have some conflicting insertion to actually test the
    -- semantic of the DB Layer.
    cond = L.length (L.nub ids) /= L.length ids
      where
        ids = map fst pairs
    setup = do
        db <- liftIO $ newDBLayer Nothing
        (cp, meta) <- pick arbitrary
        liftIO $ unsafeRunExceptT $ once_ pairs $ \(k, _) ->
            createWallet db k cp meta
        return db
    prop db = liftIO $ do
        forConcurrently_ pairs $ unsafeRunExceptT . uncurry (putOp db)
        res <- once pairs (readOp db . fst)
        length res `shouldBe` resolve pairs


{-------------------------------------------------------------------------------
                            Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary (Wallet DummyState DummyTarget) where
    shrink _ = []
    arbitrary = initWallet <$> arbitrary

data DummyTarget

instance TxId DummyTarget where
    txId = Hash . B8.pack . show

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

newtype DummyState = DummyState (UTxO, PendingTx)
    deriving (Show, Eq, NFData)

instance Arbitrary DummyState where
    shrink _ = []
    arbitrary = DummyState <$> arbitrary

instance IsOurs DummyState where
    isOurs _ wm = (True, wm)
