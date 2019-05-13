{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Cardano.Wallet.DB.Sqlite
    ( newDBLayer
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv, unXPrv, xprv )
import Cardano.Wallet.DB
    ( DBLayer (..), ErrNoSuchWallet (..), PrimaryKey (..) )
import Cardano.Wallet.DB.SqliteTypes
    ( AddressScheme (..), TxId (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Key (Key), getKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.Types
    ( WalletMetadata (..) )
import Conduit
    ( runResourceT )
import Control.DeepSeq
    ( NFData )
import Control.Exception
    ( try )
import Control.Lens
    ( to )
import Control.Monad
    ( void, (<=<) )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Logger
    ( runNoLoggingT )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Control.Monad.Trans.Reader
    ( ReaderT (..) )
import Data.Bifunctor
    ( bimap )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.List
    ( sortOn )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( UTCTime )
import Data.Word
    ( Word32 )
import Database.Persist.Sql
    ( LogFunc
    , SelectOpt (..)
    , Update (..)
    , deleteWhere
    , deleteWhereCount
    , entityVal
    , insertMany_
    , insert_
    , putMany
    , runMigration
    , runSqlConn
    , selectFirst
    , selectKeysList
    , selectList
    , updateWhere
    , (/<-.)
    , (<-.)
    , (=.)
    , (==.)
    )
import Database.Persist.Sqlite
    ( SqlBackend, SqlPersistM, wrapConnection )
import Database.Persist.TH
    ( MkPersistSettings (..)
    , mkMigrate
    , mkPersist
    , persistLowerCase
    , share
    , sqlSettings
    )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import System.IO
    ( stderr )
import System.Log.FastLogger
    ( fromLogStr )

import qualified Cardano.Wallet.Primitive.Model as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.Sqlite as Sqlite

share
    [ mkPersist sqlSettings { mpsPrefixFields = False }
    , mkMigrate "migrateAll"
    ]
    [persistLowerCase|

-- Wallet IDs, address discovery state, and metadata.
Wallet
    walTableId                 W.WalletId     sql=wallet_id
    walTableName               Text           sql=name
    walTablePassphraseLastUpdatedAt  UTCTime Maybe  sql=passphrase_last_updated_at
    walTableStatus             W.WalletState  sql=status
    walTableDelegation         Text Maybe     sql=delegation
    walTableAddressScheme      AddressScheme  sql=address_discovery

    Primary walTableId
    deriving Show Generic

-- The private key for each wallet. This is in a separate table simply so that
-- "SELECT * FROM wallet" won't print keys.
PrivateKey                               sql=private_key
    privateKeyTableWalletId  W.WalletId  sql=wallet_id
    privateKeyTableRootKey   Text        sql=root
    privateKeyTableHash      Text        sql=hash

    Primary privateKeyTableWalletId
    Foreign Wallet fk_wallet_private_key privateKeyTableWalletId

    deriving Show Generic

-- Maps a transaction ID to its metadata (which is calculated when applying
-- blocks).
--
-- TxMeta is specific to a wallet because multiple wallets may have the same
-- transaction with different metadata values. The associated inputs and outputs
-- of the transaction are in the TxIn and TxOut tables.
TxMeta
    txMetaTableTxId       TxId         sql=tx_id
    txMetaTableWalletId   W.WalletId   sql=wallet_id
    txMetaTableStatus     W.TxStatus   sql=status
    txMetaTableDirection  W.Direction  sql=direction
    txMetaTableSlotId     W.SlotId     sql=slot
    txMetaTableAmount     Natural      sql=amount

    Primary txMetaTableTxId txMetaTableWalletId
    Foreign Wallet fk_wallet_tx_meta txMetaTableWalletId
    deriving Show Generic

-- A transaction input associated with TxMeta.
--
-- There is no wallet ID because these values depend only on the transaction,
-- not the wallet. txInputTableTxId is referred to by TxMeta and PendingTx
TxIn
    txInputTableTxId         TxId        sql=tx_id
    txInputTableWalletId     W.WalletId  sql=wallet_id
    txInputTableSourceTxId   TxId        sql=source_id
    txInputTableSourceIndex  Word32      sql=source_index

    Primary txInputTableTxId txInputTableSourceTxId txInputTableSourceIndex
    Foreign TxMeta fk_tx_meta_tx_in txInputTableTxId txInputTableWalletId -- fixme: remove
    deriving Show Generic

-- A transaction output associated with TxMeta.
--
-- There is no wallet ID because these values depend only on the transaction,
-- not the wallet. txOutputTableTxId is referred to by TxMeta and PendingTx
TxOut
    txOutputTableTxId     TxId        sql=tx_id
    txOutputTableWalletId W.WalletId  sql=wallet_id
    txOutputTableIndex    Word32      sql=index
    txOutputTableAddress  W.Address   sql=address
    txOutputTableAmount   W.Coin      sql=amount

    Primary txOutputTableTxId txOutputTableIndex
    Foreign TxMeta fk_tx_meta_tx_out txOutputTableTxId txOutputTableWalletId -- fixme: remove
    deriving Show Generic

-- A checkpoint for a given wallet is referred to by (wallet_id, slot).
-- Checkpoint data such as UTxO will refer to this table.
Checkpoint
    checkpointTableWalletId    W.WalletId  sql=wallet_id
    checkpointTableSlot        W.SlotId    sql=slot

    Primary checkpointTableWalletId checkpointTableSlot
    Foreign Wallet fk_wallet_checkpoint checkpointTableWalletId

    deriving Show Generic

-- The UTxO for a given wallet checkpoint is a one-to-one mapping from TxIn ->
-- TxOut. This table does not need to refer to the TxIn or TxOut tables. All
-- necessary information for the UTxO is in this table.
UTxO                                     sql=utxo

    -- The wallet checkpoint (wallet_id, slot)
    utxoTableWalletId        W.WalletId  sql=wallet_id
    utxoTableCheckpointSlot  W.SlotId    sql=slot

    -- TxIn
    utxoTableInputId         TxId        sql=input_tx_id
    utxoTableInputIndex      Word32      sql=input_index

    -- TxOut
    utxoTableOutputAddress   W.Address   sql=output_address
    utxoTableOutputCoin      W.Coin      sql=output_coin

    Primary
        utxoTableWalletId
        utxoTableCheckpointSlot
        utxoTableInputId
        utxoTableInputIndex
        utxoTableOutputAddress
        utxoTableOutputCoin

    Foreign Checkpoint fk_checkpoint_utxo utxoTableWalletId utxoTableCheckpointSlot
    deriving Show Generic

-- The pending transactions for a wallet checkpoint.
PendingTx

    -- The wallet checkpoint (wallet_id, slot)
    pendingTxTableWalletId        W.WalletId  sql=wallet_id
    pendingTxTableCheckpointSlot  W.SlotId    sql=slot

    -- Transaction TxIn and TxOut
    pendingTxTableId2             TxId        sql=tx_id

    Primary pendingTxTableWalletId pendingTxTableCheckpointSlot pendingTxTableId2
    Foreign Checkpoint fk_pending_tx pendingTxTableWalletId pendingTxTableCheckpointSlot

|]

----------------------------------------------------------------------------
-- Sqlite connection set up

enableForeignKeys :: Sqlite.Connection -> IO ()
enableForeignKeys conn = Sqlite.prepare conn "PRAGMA foreign_keys = ON;" >>= void . Sqlite.step

createSqliteBackend :: Maybe FilePath -> LogFunc -> IO SqlBackend
createSqliteBackend fp logFunc = do
  conn <- Sqlite.open (sqliteConnStr fp)
  enableForeignKeys conn
  wrapConnection conn logFunc

sqliteConnStr :: Maybe FilePath -> Text
sqliteConnStr = maybe ":memory:" T.pack

logStderr :: LogFunc
logStderr _ _ _ str = B8.hPutStrLn stderr (fromLogStr str)

-- | Run a query without error handling. There will be exceptions thrown.
unsafeRunQuery
    :: SqlBackend
    -> SqlPersistM a
    -> IO a
unsafeRunQuery conn = runResourceT . runNoLoggingT . flip runSqlConn conn

runQuery
    :: SqlBackend
    -> SqlPersistM a
    -> ExceptT Sqlite.SqliteException IO a
runQuery conn = ExceptT . try . runResourceT . runNoLoggingT . flip runSqlConn conn


----------------------------------------------------------------------------
-- Database layer methods

-- | Sets up a connection to the SQLite database.
--
-- Database migrations are run to create tables if necessary.
--
-- If the given file path does not exist, it will be created by the sqlite
-- library.
newDBLayer
    :: forall s t. (IsOurs s, NFData s, Show s, W.TxId t)
    => Maybe FilePath
       -- ^ Database file location, or Nothing for in-memory database
    -> IO (DBLayer IO s t)
newDBLayer fp = do
    conn <- createSqliteBackend fp logStderr
    unsafeRunQuery conn $ runMigration migrateAll
    return $ DBLayer

        {-----------------------------------------------------------------------
                                      Wallets
        -----------------------------------------------------------------------}

        { createWallet = \(PrimaryKey wid) cp meta ->
            ExceptT $ unsafeRunQuery conn $ Right <$> do
                insert_ (mkWalletEntity wid meta)
                insertCheckpoint wid cp

        , removeWallet = \(PrimaryKey wid) ->
            ExceptT $ unsafeRunQuery conn $ do
                n <- deleteWhereCount [WalTableId ==. wid]
                pure $ if n == 0
                       then Left (ErrNoSuchWallet wid)
                       else Right ()
        , listWallets = unsafeRunQuery conn $
            map (PrimaryKey . unWalletKey) <$> selectKeysList [] []

        {-----------------------------------------------------------------------
                                    Checkpoints
        -----------------------------------------------------------------------}

        , putCheckpoint = \(PrimaryKey wid) cp ->
            ExceptT $ unsafeRunQuery conn $ Right <$> do
                deleteCheckpoints wid -- clear out all checkpoints
                deleteLooseTransactions wid -- clear transactions
                insertCheckpoint wid cp -- add this checkpoint

        , readCheckpoint = \(PrimaryKey wid) ->
            unsafeRunQuery conn $
            selectLatestCheckpoint wid >>= \case
                Just cp -> do
                    utxo <- selectUTxO cp
                    pendings <- selectPending cp
                    (ins, outs) <- selectTxs pendings
                    pure $ Just $ checkpointFromEntity cp utxo ins outs
                Nothing -> pure Nothing

        {-----------------------------------------------------------------------
                                   Wallet Metadata
        -----------------------------------------------------------------------}

        , putWalletMeta = \(PrimaryKey wid) meta ->
            ExceptT $ unsafeRunQuery conn $
            selectFirst [WalTableId ==. wid] [] >>= \case
                Just _ -> do
                    updateWhere [WalTableId ==. wid]
                        (mkWalletMetadataUpdate meta)
                    pure $ Right ()
                Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , readWalletMeta = \(PrimaryKey wid) -> unsafeRunQuery conn $
               fmap (metadataFromEntity . entityVal) <$> selectFirst [WalTableId ==. wid] []

        {-----------------------------------------------------------------------
                                     Tx History
        -----------------------------------------------------------------------}

        , putTxHistory = \(PrimaryKey wid) txs ->
            ExceptT $ unsafeRunQuery conn $
            selectWallet wid >>= \case
                Just _ -> do
                    let (metas, txins, txouts) = mkTxHistory wid txs
                    putMany metas
                    putMany txins
                    putMany txouts
                    pure $ Right ()
                Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , readTxHistory = \(PrimaryKey wid) -> unsafeRunQuery conn $
            selectTxHistory wid

        {-----------------------------------------------------------------------
                                       Keystore
        -----------------------------------------------------------------------}

        , putPrivateKey = \(PrimaryKey wid) key ->
            ExceptT $ unsafeRunQuery conn $
            selectWallet wid >>= \case
                Just _ -> Right <$> do
                    deleteWhere [PrivateKeyTableWalletId ==. wid]
                    insert_ (mkPrivateKeyEntity wid key)
                Nothing -> pure $ Left $ ErrNoSuchWallet wid

        , readPrivateKey = \(PrimaryKey wid) ->
            unsafeRunQuery conn $ let
                keys = selectFirst [PrivateKeyTableWalletId ==. wid] []
                toMaybe = either (const Nothing) Just
            in (>>= toMaybe . privateKeyFromEntity . entityVal) <$> keys

        {-----------------------------------------------------------------------
                                       Lock
        -----------------------------------------------------------------------}

        , withLock = \_action -> error "withLock to be implemented"

        }

----------------------------------------------------------------------------
-- Conversion between Persistent table types and wallet types

delegationToText :: W.WalletDelegation W.PoolId -> Maybe Text
delegationToText W.NotDelegating = Nothing
delegationToText (W.Delegating pool) = Just (W.getPoolId pool)

delegationFromText :: Maybe Text -> W.WalletDelegation W.PoolId
delegationFromText Nothing = W.NotDelegating
delegationFromText (Just pool) = W.Delegating (W.PoolId pool)

mkWalletEntity :: W.WalletId -> W.WalletMetadata -> Wallet
mkWalletEntity wid meta = Wallet
    { walTableId = wid
    , walTableName = meta ^. #name . to W.getWalletName
    , walTablePassphraseLastUpdatedAt = case meta ^. #passphraseInfo of
            Nothing -> Nothing
            Just (W.WalletPassphraseInfo passInfo) -> Just passInfo
    , walTableStatus = meta ^. #status
    , walTableDelegation = meta ^. #delegation . to delegationToText
    , walTableAddressScheme = Sequential -- fixme: depends on wallet
    }

mkWalletMetadataUpdate :: W.WalletMetadata -> [Update Wallet]
mkWalletMetadataUpdate meta =
    [ WalTableName =. meta ^. #name . to W.getWalletName
    , WalTablePassphraseLastUpdatedAt =. case meta ^. #passphraseInfo of
            Nothing -> Nothing
            Just (W.WalletPassphraseInfo passInfo) -> Just passInfo
    , WalTableStatus =. meta ^. #status
    , WalTableDelegation =. meta ^. #delegation . to delegationToText
    ]

metadataFromEntity :: Wallet -> W.WalletMetadata
metadataFromEntity wal = W.WalletMetadata
    { name = W.WalletName (walTableName wal)
    , passphraseInfo = case walTablePassphraseLastUpdatedAt wal of
            Just time -> Just $ W.WalletPassphraseInfo time
            Nothing -> Nothing
    , status = walTableStatus wal
    , delegation = delegationFromText (walTableDelegation wal)
    }

mkPrivateKeyUpdate
    :: (Key 'RootK XPrv, W.Hash "encryption")
    -> [Update PrivateKey]
mkPrivateKeyUpdate (root, hash) =
    let rootBS = (T.decodeUtf8 . unXPrv . getKey) root
        hashEncrypted = (T.decodeUtf8 . W.getHash) hash
    in [ PrivateKeyTableRootKey =. rootBS
       , PrivateKeyTableHash =. hashEncrypted
       ]

mkPrivateKeyEntity
    :: W.WalletId
    -> (Key 'RootK XPrv, W.Hash "encryption")
    -> PrivateKey
mkPrivateKeyEntity wid (root, hash) = PrivateKey
    { privateKeyTableWalletId = wid
    , privateKeyTableRootKey = hexText . unXPrv . getKey $ root
    , privateKeyTableHash = hexText . W.getHash $ hash
    }
  where
    hexText = T.decodeUtf8 . convertToBase Base16

privateKeyFromEntity
    :: PrivateKey
    -> Either String (Key 'RootK XPrv, W.Hash "encryption")
privateKeyFromEntity (PrivateKey _ k h) = (,)
    <$> fmap Key (xprvFromText k)
    <*> fmap W.Hash (fromHexText h)
  where
    fromHexText :: Text -> Either String B8.ByteString
    fromHexText = convertFromBase Base16 . T.encodeUtf8
    xprvFromText = xprv <=< fromHexText

mkCheckpointEntity
    :: forall s t. W.TxId t
    => W.WalletId
    -> W.Wallet s t
    -> (Checkpoint, [UTxO], [PendingTx], [TxIn], [TxOut])
mkCheckpointEntity wid wal =
    ( cp, utxo, map (pendingTx . fst) pending
    , concatMap (dist pendingTxIn . fmap W.inputs) pending
    , concatMap (dist pendingTxOut . fmap (zip [0..] . W.outputs)) pending )
  where
    pending = [(TxId (W.txId @t tx), tx) | tx <- Set.toList (W.getPending wal)]
    sl = W.currentTip wal
    cp = Checkpoint
        { checkpointTableWalletId = wid
        , checkpointTableSlot = sl
        }
    pendingTx tid = PendingTx
        { pendingTxTableWalletId = wid
        , pendingTxTableCheckpointSlot = sl
        , pendingTxTableId2 = tid
        }
    pendingTxIn tid txIn = TxIn
        { txInputTableTxId = tid
        , txInputTableWalletId = wid
        , txInputTableSourceTxId = TxId (W.inputId txIn)
        , txInputTableSourceIndex = W.inputIx txIn
        }
    pendingTxOut tid (ix, txOut) = TxOut
        { txOutputTableTxId = tid
        , txOutputTableWalletId = wid
        , txOutputTableIndex = ix
        , txOutputTableAddress = W.address txOut
        , txOutputTableAmount = W.coin txOut
        }
    utxo = [ UTxO wid sl (TxId input) ix addr coin
           | (W.TxIn input ix, W.TxOut addr coin) <- utxoMap ]
    utxoMap = Map.assocs (W.getUTxO (W.totalUTxO wal))

dist :: (a -> b -> c) -> (a, [b]) -> [c]
dist f (a, bs) = [f a b | b <- bs]

-- inputs and outputs must be sorted by txid, then ix
checkpointFromEntity
    :: forall s t. (IsOurs s, NFData s, Show s, W.TxId t)
    => Checkpoint
    -> [UTxO]
    -> [TxIn]
    -> [TxOut]
    -> W.Wallet s t
checkpointFromEntity (Checkpoint _ tip) utxo ins outs =
    W.Wallet utxo' pending tip s
  where
    utxo' = W.UTxO . Map.fromList $
        [ (W.TxIn input ix, W.TxOut addr coin)
        | UTxO _ _ (TxId input) ix addr coin <- utxo ]
    ins' = [(txid, W.TxIn src ix) | TxIn txid _ (TxId src) ix <- ins]
    outs' = [ (txid, (ix, W.TxOut addr amt))
            | TxOut txid _ ix addr amt <- outs ]
    txids = Set.fromList $ map fst ins' ++ map fst outs'
    pending = flip Set.map txids $ \txid -> W.Tx
        { W.inputs = lookupTx txid ins'
        , W.outputs = ordered . lookupTx txid $ outs'
        }
    lookupTx txid = map snd . filter ((== txid) . fst)
    -- fixme: sorting not necessary if db query was ordered
    ordered = map snd . sortOn fst
    s = error "fixme: implement wallet state in sqlite"

mkTxHistory
    :: W.WalletId
    -> Map.Map (W.Hash "Tx") (W.Tx, W.TxMeta)
    -> ([TxMeta], [TxIn], [TxOut])
mkTxHistory wid txs =
    ( map (uncurry (mkTxMetaEntity wid)) metas
    , concatMap (dist mkTxIn . fmap W.inputs) hist
    , concatMap (dist mkTxOut . fmap (zip [0..] . W.outputs)) hist )
  where
    pairs = Map.toList txs
    metas = fmap snd <$> pairs
    hist = bimap TxId fst <$> pairs
    mkTxIn tid txIn = TxIn
        { txInputTableTxId = tid
        , txInputTableWalletId = wid
        , txInputTableSourceTxId = TxId (W.inputId txIn)
        , txInputTableSourceIndex = W.inputIx txIn
        }
    mkTxOut tid (ix, txOut) = TxOut
        { txOutputTableTxId = tid
        , txOutputTableWalletId = wid
        , txOutputTableIndex = ix
        , txOutputTableAddress = W.address txOut
        , txOutputTableAmount = W.coin txOut
        }

mkTxMetaEntity :: W.WalletId -> W.Hash "Tx" -> W.TxMeta -> TxMeta
mkTxMetaEntity wid txid meta = TxMeta
    { txMetaTableTxId = TxId txid
    , txMetaTableWalletId = wid
    , txMetaTableStatus = meta ^. #status
    , txMetaTableDirection = meta ^. #direction
    , txMetaTableSlotId = meta ^. #slotId
    , txMetaTableAmount = getAmount (meta ^. #amount)
    }
    where getAmount (Quantity n) = n

-- note: TxOut records must already be sorted by index
txHistoryFromEntity
    :: [TxMeta]
    -> [TxIn]
    -> [TxOut]
    -> Map.Map (W.Hash "Tx") (W.Tx, W.TxMeta)
txHistoryFromEntity metas ins outs = Map.fromList
    [ (getTxId (txMetaTableTxId m), (mkTx (txMetaTableTxId m), mkTxMeta m))
    | m <- metas ]
  where
    mkTx txid = W.Tx
        { W.inputs = map mkTxIn $ filter ((== txid) . txInputTableTxId) ins
        , W.outputs = map mkTxOut $ filter ((== txid) . txOutputTableTxId) outs
        }
    mkTxIn tx = W.TxIn
        { W.inputId = getTxId (txInputTableSourceTxId tx)
        , W.inputIx = txInputTableSourceIndex tx
        }
    mkTxOut tx = W.TxOut
        { W.address = txOutputTableAddress tx
        , W.coin = txOutputTableAmount tx
        }
    mkTxMeta m = W.TxMeta
        { W.status = txMetaTableStatus m
        , W.direction = txMetaTableDirection m
        , W.slotId = txMetaTableSlotId m
        , W.amount = Quantity (txMetaTableAmount m)
        }

----------------------------------------------------------------------------
-- DB Queries

selectWallet :: MonadIO m => W.WalletId -> ReaderT SqlBackend m (Maybe Wallet)
selectWallet wid = fmap entityVal <$> selectFirst [WalTableId ==. wid] []

insertCheckpoint
    :: (MonadIO m, W.TxId t)
    => W.WalletId
    -> W.Wallet s t
    -> ReaderT SqlBackend m ()
insertCheckpoint wid cp = do
    let (cp', utxo, pendings, ins, outs) = mkCheckpointEntity wid cp
    insert_ cp'
    insertMany_ ins
    insertMany_ outs
    insertMany_ pendings
    insertMany_ utxo

-- | Delete all checkpoints associated with a wallet.
deleteCheckpoints
    :: MonadIO m
    => W.WalletId
    -> ReaderT SqlBackend m ()
deleteCheckpoints wid = do
    deleteWhere [UtxoTableWalletId ==. wid]
    deleteWhere [PendingTxTableWalletId ==. wid]
    deleteWhere [CheckpointTableWalletId ==. wid]

-- | Delete transactions that belong to a wallet and aren't referred to by
-- either Pending or TxMeta.
deleteLooseTransactions
    :: MonadIO m
    => W.WalletId
    -> ReaderT SqlBackend m ()
deleteLooseTransactions wid = do
    pendingTxId <- fmap (pendingTxTableId2 . entityVal) <$>
        selectList [PendingTxTableWalletId ==. wid] []
    metaTxId <- fmap (txMetaTableTxId . entityVal) <$>
        selectList [TxMetaTableWalletId ==. wid] []
    deleteWhere [ TxInputTableWalletId ==. wid
                , TxInputTableTxId /<-. pendingTxId
                , TxInputTableTxId /<-. metaTxId ]
    deleteWhere [ TxOutputTableWalletId ==. wid
                , TxOutputTableTxId /<-. pendingTxId
                , TxOutputTableTxId /<-. metaTxId ]

selectLatestCheckpoint
    :: MonadIO m
    => W.WalletId
    -> ReaderT SqlBackend m (Maybe Checkpoint)
selectLatestCheckpoint wid = fmap entityVal <$>
    selectFirst [CheckpointTableWalletId ==. wid]
    [LimitTo 1, Desc CheckpointTableSlot]

selectUTxO
    :: MonadIO m
    => Checkpoint
    -> ReaderT SqlBackend m [UTxO]
selectUTxO (Checkpoint wid sl) = fmap entityVal <$>
    selectList [UtxoTableWalletId ==. wid, UtxoTableCheckpointSlot ==. sl] []

selectPending
    :: MonadIO m
    => Checkpoint
    -> ReaderT SqlBackend m [TxId]
selectPending (Checkpoint wid sl) = fmap (pendingTxTableId2 . entityVal) <$>
    selectList [ PendingTxTableWalletId ==. wid
               , PendingTxTableCheckpointSlot ==. sl ] []

selectTxs
    :: MonadIO m
    => [TxId]
    -> ReaderT SqlBackend m ([TxIn], [TxOut])
selectTxs txids = do
    ins <- fmap entityVal <$> selectList [TxInputTableTxId <-. txids]
        [Asc TxInputTableTxId, Asc TxInputTableSourceIndex]
    outs <- fmap entityVal <$> selectList [TxOutputTableTxId <-. txids]
        [Asc TxOutputTableTxId, Asc TxOutputTableIndex]
    pure (ins, outs)

selectTxHistory
    :: MonadIO m
    => W.WalletId
    -> ReaderT SqlBackend m (Map.Map (W.Hash "Tx") (W.Tx, W.TxMeta))
selectTxHistory wid = do
    metas <- fmap entityVal <$> selectList [TxMetaTableWalletId ==. wid] []
    let txids = map txMetaTableTxId metas
    ins <- fmap entityVal <$> selectList [TxInputTableTxId <-. txids]
        [Asc TxInputTableTxId, Asc TxInputTableSourceIndex]
    outs <- fmap entityVal <$> selectList [TxOutputTableTxId <-. txids]
        [Asc TxOutputTableTxId, Asc TxOutputTableIndex]
    pure $ txHistoryFromEntity metas ins outs
