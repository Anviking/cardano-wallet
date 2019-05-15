{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DBSpec2
    ( spec
    ) where

import Prelude hiding
    ( elem )

import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable
    ( toList )
import Data.Functor.Classes
import Data.Maybe
    ( fromJust, fromMaybe )
import Data.TreeDiff
    ( ToExpr (..), defaultExprViaShow )

import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Key, XPrv )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet, initWallet )
import Cardano.Wallet.Primitive.Types
    ( Hash (..)
    , Tx (..)
    , TxId (..)
    , TxMeta (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletState (..)
    )
import Control.Concurrent.MVar
    ( modifyMVar, newMVar )
import Control.Monad.IO.Class
    ( liftIO )
import Crypto.Hash
    ( hash )

import Control.DeepSeq
    ( NFData )
import Data.Map
    ( Map )

import GHC.Generics
    ( Generic )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), Gen, Property, quickCheck )

import Cardano.Wallet.DB.TestWallet

import qualified Data.ByteString.Char8 as B8

import qualified Cardano.Wallet.DB.MVar as MVar
import qualified Data.Bifunctor.TH as TH
import qualified Data.Map as Map
import qualified Data.Text as T

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
-- import qualified Test.QuickCheck.Random as QC

import Test.StateMachine
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2

{-------------------------------------------------------------------------------
  WalletId expressions
-------------------------------------------------------------------------------}

data Expr fp =
    Val MWid
  | Var fp
  deriving (Show, Functor, Foldable, Traversable)

eval :: Expr MWid -> MWid
eval (Val f) = f
eval (Var f) = f

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data Err
    = NoSuchWallet MWid
    | WalletAlreadyExists MWid
    deriving (Show, Eq)

errNoSuchWallet :: ErrNoSuchWallet -> Err
errNoSuchWallet (ErrNoSuchWallet wid) = NoSuchWallet (MWid wid)

errWalletAlreadyExists :: ErrWalletAlreadyExists -> Err
errWalletAlreadyExists (ErrWalletAlreadyExists wid) = WalletAlreadyExists (MWid wid)

{-------------------------------------------------------------------------------
  Mock implementation
-------------------------------------------------------------------------------}

-- fixme: remove MWid silliness
newtype MWid = MWid WalletId
    deriving (Show, Eq, Ord, Generic)

type MWallet = Wallet DummyState DummyTarget
type MPrivKey = (String, String)

newtype MConn = MConn Int
    deriving (Show, Eq, Ord, Generic)

instance Enum MConn where
    toEnum = MConn
    fromEnum (MConn n) = n

widPK :: MWid -> PrimaryKey WalletId
widPK (MWid wid) = PrimaryKey wid

widPK' :: Expr MWid -> PrimaryKey WalletId
widPK' expr = widPK (eval expr)

pkWid :: PrimaryKey WalletId -> MWid
pkWid (PrimaryKey wid) = MWid wid

data Mock = M
    { checkpoints :: Map MWid MWallet
    , metas :: Map MWid WalletMetadata
    , txs :: Map MWid TxHistory
    , privateKey :: Map MWid MPrivKey
    , next :: MConn
    }
    deriving (Show, Generic)

emptyMock :: Mock
emptyMock = M Map.empty Map.empty Map.empty Map.empty (MConn 0)

type MockOp a = Mock -> (Either Err a, Mock)

mOpen :: MockOp MConn
mOpen (M cp metas txs pk n) = (Right n, M cp metas txs pk (succ n))

mClose :: MConn -> MockOp ()
mClose _ m = (Right (), m)

mCreateWallet :: MWid -> MWallet -> WalletMetadata -> MockOp ()
mCreateWallet wid wal meta m@(M cp metas txs pk n)
    | wid `Map.member` cp = (Left (WalletAlreadyExists wid), m)
    | otherwise           = (Right (), M (Map.insert wid wal cp) (Map.insert wid meta metas) txs pk n)

mRemoveWallet :: MWid -> MockOp ()
mRemoveWallet wid m@(M cp metas txs pk n)
    | wid `Map.member` cp = (Right (), M (Map.delete wid cp) (Map.delete wid metas) txs pk n)
    | otherwise           = (Left (NoSuchWallet wid), m)

mListWallets :: MockOp [MWid]
mListWallets m@(M cp _ _ _ _) = (Right (Map.keys cp), m)

mPutCheckpoint :: MWid -> MWallet -> MockOp ()
mPutCheckpoint wid wal m@(M cp metas txs pk n)
    | wid `Map.member` cp = (Right (), M (Map.insert wid wal cp) metas txs pk n)
    | otherwise           = (Left (NoSuchWallet wid), m)

mReadCheckpoint :: MWid -> MockOp (Maybe MWallet)
mReadCheckpoint wid m@(M cp _ _ _ _)
    = (Right (Map.lookup wid cp), m)

mPutWalletMeta :: MWid -> WalletMetadata -> MockOp ()
mPutWalletMeta wid meta m@(M cp metas txs pk n)
    | wid `Map.member` cp = (Right (), M cp (Map.insert wid meta metas) txs pk n)
    | otherwise           = (Left (NoSuchWallet wid), m)

mReadWalletMeta :: MWid -> MockOp (Maybe WalletMetadata)
mReadWalletMeta wid m@(M _ meta _ _ _)
    = (Right (Map.lookup wid meta), m)

mPutTxHistory :: MWid -> TxHistory -> MockOp ()
mPutTxHistory wid txs' m@(M cp metas txs pk n)
    | wid `Map.member` cp = (Right (), M cp metas (Map.alter appendTxs wid txs) pk n)
    | otherwise           = (Left (NoSuchWallet wid), m)
    where appendTxs = Just . (<> txs') . fromMaybe mempty

mReadTxHistory :: MWid -> MockOp TxHistory
mReadTxHistory wid m@(M cp _ txs _ _)
    | wid `Map.member` cp = (Right (fromMaybe mempty (Map.lookup wid txs)), m)
    | otherwise           = (Right mempty, m)

mPutPrivateKey :: MWid -> MPrivKey -> MockOp ()
mPutPrivateKey wid pk' m@(M cp metas txs pk n)
    | wid `Map.member` cp = (Right (), M cp metas txs (Map.insert wid pk' pk) n)
    | otherwise           = (Left (NoSuchWallet wid), m)

mReadPrivateKey :: MWid -> MockOp (Maybe MPrivKey)
mReadPrivateKey wid m@(M cp _ _ pk _)
    | wid `Map.member` cp = (Right (Map.lookup wid pk), m)
    | otherwise           = (Left (NoSuchWallet wid), m)

{-------------------------------------------------------------------------------
  Language
-------------------------------------------------------------------------------}

type TxHistory = Map (Hash "Tx") (Tx, TxMeta)

data Cmd wid conn
    = Open
    | Close conn
    | CreateWallet conn MWid MWallet WalletMetadata -- fixme: MWid -> wid
    | RemoveWallet conn (Expr wid)
    | ListWallets conn
    | PutCheckpoint conn (Expr wid) MWallet
    | ReadCheckpoint conn (Expr wid)
    | PutWalletMeta conn (Expr wid) WalletMetadata
    | ReadWalletMeta conn (Expr wid)
    | PutTxHistory conn (Expr wid) TxHistory
    | ReadTxHistory conn (Expr wid)
    | PutPrivateKey conn (Expr wid) MPrivKey
    | ReadPrivateKey conn (Expr wid)
  deriving (Show, Functor, Foldable, Traversable)

data Success wid conn =
    Unit ()
  | Connection conn
  | NewWallet wid
  | WalletIds [wid]
  | Checkpoint (Maybe MWallet)
  | Metadata (Maybe WalletMetadata)
  | TxHistory TxHistory
  | PrivateKey (Maybe MPrivKey)
  deriving (Show, Eq, Functor, Foldable, Traversable)

newtype Resp wid conn = Resp (Either Err (Success wid conn))
  deriving (Show, Eq, Functor, Foldable, Traversable)

TH.deriveBifunctor     ''Cmd
TH.deriveBifoldable    ''Cmd
TH.deriveBitraversable ''Cmd

TH.deriveBifunctor     ''Success
TH.deriveBifoldable    ''Success
TH.deriveBitraversable ''Success

TH.deriveBifunctor     ''Resp
TH.deriveBifoldable    ''Resp
TH.deriveBitraversable ''Resp

{-------------------------------------------------------------------------------
  Interpreter: mock implementation
-------------------------------------------------------------------------------}

runMock :: Cmd MWid MConn -> Mock -> (Resp MWid MConn, Mock)
runMock (Open)                        = first (Resp . fmap Connection) . mOpen
runMock (Close c)                     = first (Resp . fmap Unit)       . mClose c
runMock (CreateWallet _ wid wal meta) = first (Resp . fmap (const (NewWallet wid))) . mCreateWallet wid wal meta
runMock (RemoveWallet _ wid)          = first (Resp . fmap Unit)       . mRemoveWallet (eval wid)
runMock (ListWallets _)               = first (Resp . fmap WalletIds)  . mListWallets
runMock (PutCheckpoint _ wid wal)     = first (Resp . fmap Unit)       . mPutCheckpoint (eval wid) wal
runMock (ReadCheckpoint _ wid)        = first (Resp . fmap Checkpoint) . mReadCheckpoint (eval wid)
runMock (PutWalletMeta _ wid meta)    = first (Resp . fmap Unit)       . mPutWalletMeta (eval wid) meta
runMock (ReadWalletMeta _ wid)        = first (Resp . fmap Metadata)   . mReadWalletMeta (eval wid)
runMock (PutTxHistory _ wid txs)      = first (Resp . fmap Unit)       . mPutTxHistory (eval wid) txs
runMock (ReadTxHistory _ wid)         = first (Resp . fmap TxHistory)  . mReadTxHistory (eval wid)
runMock (PutPrivateKey _ wid pk)      = first (Resp . fmap Unit)       . mPutPrivateKey (eval wid) pk
runMock (ReadPrivateKey _ wid)        = first (Resp . fmap PrivateKey) . mReadPrivateKey (eval wid)

{-------------------------------------------------------------------------------
  Interpreter: real I/O
-------------------------------------------------------------------------------}

type WPrivKey = (Key 'RootK XPrv, Hash "encryption")

-- Number the db layers so that they can be looked up
data DBLayerTest = DBLayerTest
    { dbLayerHandle :: Int
    , dbLayer :: DBLayer IO DummyState DummyTarget
    } deriving (Generic)

instance Show DBLayerTest where
    show (DBLayerTest h _) = "DBLayerTest " ++ show h

instance Eq DBLayerTest where
    (DBLayerTest a _) == (DBLayerTest b _) = a == b

{-
mkConnectSqlite :: Maybe FilePath -> IO (IO DBLayerTest)
mkConnect dbFile = do
    mv <- newMVar []
    pure $ modifyMVar mv $ \conns -> do
        let next = if null conns then 0
                else maximum (map dbLayerHandle conns) + 1
        conn <- DBLayerTest next <$> Sqlite.newDBLayer dbFile
        pure ((conn:conns), conn)
-}

mkConnectMVar :: Maybe FilePath -> IO (IO DBLayerTest)
mkConnectMVar _dbFile = do
    mv <- newMVar []
    db <- MVar.newDBLayer -- share the same db for all connections
    pure $ modifyMVar mv $ \conns -> do
        let conn = DBLayerTest (nextHandle conns) db
        pure ((conn:conns), conn)

mkConnect :: Maybe FilePath -> IO (IO DBLayerTest)
mkConnect = mkConnectMVar

nextHandle :: [DBLayerTest] -> Int
nextHandle [] = 0
nextHandle conns = maximum (map dbLayerHandle conns) + 1

runIO
    :: IO DBLayerTest
    -> Cmd MWid DBLayerTest
    -> IO (Resp MWid DBLayerTest)
runIO connect = fmap Resp . go
  where
    go :: Cmd MWid DBLayerTest -> IO (Either Err (Success MWid DBLayerTest))
    go (Open) = Right . Connection <$> connect
    go (Close _db) = pure (Right (Unit ()))
    go (CreateWallet db wid wal meta) = bimap errWalletAlreadyExists Unit <$> runExceptT (createWallet (dbLayer db) (widPK wid) wal meta)
    go (RemoveWallet db wid) = catchNoSuchWallet Unit $ removeWallet (dbLayer db) (widPK' wid)
    go (ListWallets db) = Right . WalletIds . fmap pkWid <$> listWallets (dbLayer db)
    go (PutCheckpoint db wid wal) = catchNoSuchWallet Unit $ putCheckpoint (dbLayer db) (widPK' wid) wal
    go (ReadCheckpoint db wid) = Right . Checkpoint <$> readCheckpoint (dbLayer db) (widPK' wid)
    go (PutWalletMeta db wid meta) = catchNoSuchWallet Unit $ putWalletMeta (dbLayer db) (widPK' wid) meta
    go (ReadWalletMeta db wid) = Right . Metadata <$> readWalletMeta (dbLayer db) (widPK' wid)
    go (PutTxHistory db wid txs) = catchNoSuchWallet Unit $ putTxHistory (dbLayer db) (widPK' wid) txs
    go (ReadTxHistory db wid) = Right . TxHistory <$> readTxHistory (dbLayer db) (widPK' wid)
    -- go (PutPrivateKey db wid pk) = catchNoSuchWallet Unit $ putPrivateKey (dbLayer db) (widPK' wid) pk
    -- go (ReadPrivateKey db wid) = Right . PrivateKey <$> readPrivateKey (dbLayer db) (widPK' wid)
    go (PutPrivateKey{}) = error "todo PutPrivateKey"
    go (ReadPrivateKey{}) = error "todo ReadPrivateKey"

    catchNoSuchWallet f = fmap (bimap errNoSuchWallet f) . runExceptT

{-------------------------------------------------------------------------------
  Working with references
-------------------------------------------------------------------------------}

newtype At f r = At (f (Reference MWid r) (Reference DBLayerTest r))

deriving instance Show (f (Reference MWid r) (Reference DBLayerTest r)) => Show (At f r)

type f :@ r = At f r

type RefEnv k a r = [(Reference k r, a)]

(!) :: (Eq1 r, Eq k) => RefEnv k a r -> Reference k r -> a
env ! r = fromJust (lookup r env)

{-------------------------------------------------------------------------------
  Relating the mock model to the real implementation
-------------------------------------------------------------------------------}

type ConnRefs r = RefEnv DBLayerTest MConn r
type WidRefs  r = RefEnv MWid        MWid  r -- fixme: WalletId -> MWid

data Model r = Model Mock (WidRefs r) (ConnRefs r)
  deriving (Generic)

deriving instance Show1 r => Show (Model r)

initModel :: Model r
initModel = Model emptyMock [] []

toMock :: (Bifunctor f, Eq1 r) => Model r -> f :@ r -> f MWid MConn
toMock (Model _ wids conns) (At fr) = bimap (wids !) (conns !) fr

step :: Eq1 r => Model r -> Cmd :@ r -> (Resp MWid MConn, Mock)
step m@(Model mock _ _) c = runMock (toMock m c) mock

{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

data Event r = Event {
    before   :: Model  r
  , cmd      :: Cmd :@ r
  , after    :: Model  r
  , mockResp :: Resp MWid MConn
  }

deriving instance Show1 r => Show (Event r)

lockstep :: forall r. Eq1 r
         => Model   r
         -> Cmd  :@ r
         -> Resp :@ r
         -> Event   r
lockstep m@(Model _ ws cs) c (At resp) = Event {
      before   = m
    , cmd      = c
    , after    = Model mock' (ws <> ws') (cs <> cs')
    , mockResp = resp'
    }
  where
    (resp', mock') = step m c
    ws' :: WidRefs r
    ws' = zip (toList1 resp) (toList1 resp')
    cs' = zip (toList2 resp) (toList2 resp')

toList1 :: Bifoldable t => t a b -> [a]
toList1 = bifoldMap (:[]) (const [])

toList2 :: Bifoldable t => t a b -> [b]
toList2 = bifoldMap (const []) (:[])

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

generator :: Model Symbolic -> Maybe (Gen (Cmd :@ Symbolic))
generator (Model _ wids conns) = Just $ QC.oneof $ concat
    [ withoutConn
    , if null conns then [] else withConn
    ]
  where
    withoutConn :: [Gen (Cmd :@ Symbolic)]
    withoutConn = [
          pure (At Open)
        ]

    withConn :: [Gen (Cmd :@ Symbolic)]
    withConn = concat
        [ withoutWid
        , if null wids then [] else withWid
        ]

    withoutWid :: [Gen (Cmd :@ Symbolic)]
    withoutWid =
        [ fmap At $ Close <$> genConn
        , fmap At $ CreateWallet <$> genConn <*> genId <*> arbitrary <*> arbitrary
        ]

    withWid :: [Gen (Cmd :@ Symbolic)]
    withWid =
        [ fmap At $ RemoveWallet <$> genConn <*> genId'
        , fmap At $ ListWallets <$> genConn
        , fmap At $ PutCheckpoint <$> genConn <*> genId' <*> arbitrary
        , fmap At $ ReadCheckpoint <$> genConn <*> genId'
        , fmap At $ PutWalletMeta <$> genConn <*> genId' <*> arbitrary
        , fmap At $ ReadWalletMeta <$> genConn <*> genId'
        -- , fmap At $ PutTxHistory <$> genConn <*> genId <*> genTxHistory
        , fmap At $ ReadTxHistory <$> genConn <*> genId'
        -- , fmap At $ PutPrivateKey <$> genId <*> genPrivKey
        -- , fmap At $ ReadPrivateKey <$> genId
        ]

    genId :: Gen MWid
    genId = MWid . WalletId . hash . B8.pack <$> QC.elements ["a", "b", "c"]

    genId' = Val <$> genId

    -- genUsedId :: Gen (Reference MWid Symbolic)
    -- genUsedId = QC.elements (map fst wids)

    genConn :: Gen (Reference DBLayerTest Symbolic)
    genConn = QC.elements (map fst conns)

    -- genTxHistory :: Gen TxHistory
    -- genTxHistory = arbitrary

shrinker :: Model Symbolic -> Cmd :@ Symbolic -> [Cmd :@ Symbolic]
shrinker _ _ = [] -- fixme: shrinker


{-------------------------------------------------------------------------------
  The state machine proper
-------------------------------------------------------------------------------}

transition :: Eq1 r => Model r -> Cmd :@ r -> Resp :@ r -> Model r
transition m c = after . lockstep m c

precondition :: Model Symbolic -> Cmd :@ Symbolic -> Logic
precondition (Model _ wids conns) (At c) =
        forall (toList1 c) (`elem` map fst wids)
    :&& forall (toList2 c) (`elem` map fst conns)

postcondition :: Model Concrete -> Cmd :@ Concrete -> Resp :@ Concrete -> Logic
postcondition m c r =
    toMock (after e) r .== mockResp e
  where
    e = lockstep m c r

semantics :: IO DBLayerTest -> Cmd :@ Concrete -> IO (Resp :@ Concrete)
semantics connect (At c) =
    (At . bimap QSM.reference QSM.reference) <$>
      runIO connect (bimap QSM.concrete QSM.concrete c)

symbolicResp :: Model Symbolic -> Cmd :@ Symbolic -> GenSym (Resp :@ Symbolic)
symbolicResp m c = At <$> bitraverse (const QSM.genSym) (const QSM.genSym) resp
  where
    (resp, _mock') = step m c

sm :: IO DBLayerTest -> StateMachine Model (At Cmd) IO (At Resp)
sm connect = QSM.StateMachine {
      initModel     = initModel
    , transition    = transition
    , precondition  = precondition
    , postcondition = postcondition
    , invariant     = Nothing
    , generator     = generator
    , distribution  = Nothing
    , shrinker      = shrinker
    , semantics     = semantics connect
    , mock          = symbolicResp
    }

{-------------------------------------------------------------------------------
  Additional type class instances required to run the QSM tests
-------------------------------------------------------------------------------}

instance CommandNames (At Cmd) where
  cmdName (At Open{})           = "Open"
  cmdName (At Close{})          = "Close"
  cmdName (At CreateWallet{})   = "CreateWallet"
  cmdName (At RemoveWallet{})   = "RemoveWallet"
  cmdName (At ListWallets{})    = "ListWallets"
  cmdName (At PutCheckpoint{})  = "PutCheckpoint"
  cmdName (At ReadCheckpoint{}) = "ReadCheckpoint"
  cmdName (At PutWalletMeta{})  = "PutWalletMeta"
  cmdName (At ReadWalletMeta{}) = "ReadWalletMeta"
  cmdName (At PutTxHistory{})   = "PutTxHistory"
  cmdName (At ReadTxHistory{})  = "ReadTxHistory"
  cmdName (At PutPrivateKey{})  = "PutPrivateKey"
  cmdName (At ReadPrivateKey{}) = "ReadPrivateKey"
  cmdNames _ = ["Open", "Close", "CreateWallet", "CreateWallet", "RemoveWallet", "ListWallets", "PutCheckpoint", "ReadCheckpoint", "PutWalletMeta", "ReadWalletMeta", "PutTxHistory", "ReadTxHistory", "PutPrivateKey", "ReadPrivateKey"]

instance Bifunctor f => Rank2.Functor (At f) where
  fmap = \f (At x) -> At $ bimap (lift f) (lift f) x
    where
      lift :: (r x -> r' x) -> QSM.Reference x r -> QSM.Reference x r'
      lift f (QSM.Reference x) = QSM.Reference (f x)

instance Bifoldable f => Rank2.Foldable (At f) where
  foldMap = \f (At x) -> bifoldMap (lift f) (lift f) x
    where
      lift :: (r x -> m) -> QSM.Reference x r -> m
      lift f (QSM.Reference x) = f x

instance Bitraversable t => Rank2.Traversable (At t) where
  traverse = \f (At x) -> At <$> bitraverse (lift f) (lift f) x
    where
      lift :: Functor f
           => (r x -> f (r' x)) -> QSM.Reference x r -> f (QSM.Reference x r')
      lift f (QSM.Reference x) = QSM.Reference <$> f x

deriving instance ToExpr (Model Concrete)
deriving instance ToExpr Mock
deriving instance ToExpr MConn

instance ToExpr DBLayerTest where
    toExpr = defaultExprViaShow
    -- toExpr = App "DBLayerTest" []
instance ToExpr MWallet where
    toExpr = defaultExprViaShow
    -- toExpr = App "Wallet DummyState DummyTarget" []

instance ToExpr WalletMetadata where
    toExpr = defaultExprViaShow

instance ToExpr (Hash "Tx") where
    toExpr = defaultExprViaShow

instance ToExpr Tx where
    toExpr = defaultExprViaShow

instance ToExpr TxMeta where
    toExpr = defaultExprViaShow

instance ToExpr MWid where
    toExpr = defaultExprViaShow

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

spec :: Spec
spec = describe "DBLayer state machine tests" $ do
    it "Sequential tests" $
        quickCheck (prop_sequential Nothing)


prop_sequential :: Maybe FilePath -> Property
prop_sequential dbFile =
    forAllCommands (sm dbFileUnused) Nothing $ \cmds ->
    QC.monadicIO $ do
        connect <- liftIO $ mkConnect dbFile
        let sm' = sm connect
        (hist, _model, res) <- runCommands sm' cmds
        prettyCommands sm' hist
          $ checkCommandNames cmds
          $ res QC.=== Ok

dbFileUnused :: IO DBLayerTest
dbFileUnused = error "db file not used during command generation"
