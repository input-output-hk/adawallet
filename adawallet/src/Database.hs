module Database where

import Database.Sqlite
import Data.Text (Text)
import DB.Schema
import Prelude
-- import Cardano.Prelude
import Database.Persist.Sql
import Control.Monad.Logger
import Database.Persist.Sqlite
import Control.Monad.IO.Unlift
       ( MonadIO(..)
       , MonadUnliftIO)
import Control.Monad.Trans.Reader (ReaderT)
import System.IO
import Prelude

-------------------------------------------------------------------------------
-- Run
-------------------------------------------------------------------------------

withConnectionDebug :: (MonadIO m, MonadUnliftIO m) => (SqlBackend -> (LoggingT m) a) -> m a
withConnectionDebug = withConnection' True

withConnection :: (MonadIO m, MonadUnliftIO m) => (SqlBackend -> (LoggingT m) a) -> m a
withConnection = withConnection' False

withConnection' :: (MonadIO m, MonadUnliftIO m) => Bool -> (SqlBackend -> (LoggingT m) a) -> m a
withConnection' shouldLog action =
    runLoggingT action' logAction
  where
    logAction
      | shouldLog = defaultOutput stdout
      | otherwise = \_ _ _ _ -> pure ()

    action' = withSqliteConn ":memory:" action

-------------------------------------------------------------------------------
-- Insert
-------------------------------------------------------------------------------

insertState :: (MonadIO m, MonadUnliftIO m) => State -> SqlBackend -> m StateId
insertState = runSqlConn . insert

insertStateUtxo :: (MonadIO m, MonadUnliftIO m) => Utxo -> SqlBackend -> m UtxoId
insertStateUtxo = runSqlConn . insert

insertAccount :: (MonadIO m, MonadUnliftIO m) => Account -> SqlBackend -> m AccountId
insertAccount = runSqlConn . insert

insertManyState :: (MonadIO m, MonadUnliftIO m) => [State] -> SqlBackend -> m [StateId]
insertManyState = runSqlConn . insertMany

insertManyStateUtxo :: (MonadIO m, MonadUnliftIO m) => [Utxo] -> SqlBackend -> m [UtxoId]
insertManyStateUtxo = runSqlConn . insertMany

insertManyAccount :: (MonadIO m, MonadUnliftIO m) => [Account] -> SqlBackend -> m [AccountId]
insertManyAccount = runSqlConn . insertMany


-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------
