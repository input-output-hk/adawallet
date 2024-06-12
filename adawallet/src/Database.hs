{-# LANGUAGE TypeApplications #-}

module Database where

import DB.Schema
import Data.Text (Text)
import Database.Sqlite

import Cardano.Prelude (listToMaybe)
import Control.Monad.IO.Unlift (
  MonadIO (..),
  MonadUnliftIO,
 )
import Control.Monad.Logger
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Persist.Class.PersistQuery
import Database.Persist.Sql
import Database.Persist.Sql (Entity, deleteWhere, selectList)
import Database.Persist.Sqlite
import System.IO
import Prelude

-------------------------------------------------------------------------------
-- Run
-------------------------------------------------------------------------------

withConnectionDebug ::
  (MonadIO m, MonadUnliftIO m) => String -> (SqlBackend -> (LoggingT m) a) -> m a
withConnectionDebug fp = withConnection' fp True

withConnection :: (MonadIO m, MonadUnliftIO m) => String -> (SqlBackend -> (LoggingT m) a) -> m a
withConnection fp = withConnection' fp False

withConnection' ::
  (MonadIO m, MonadUnliftIO m) => String -> Bool -> (SqlBackend -> (LoggingT m) a) -> m a
withConnection' fp shouldLog action =
  runLoggingT action' logAction
  where
    logAction
      | shouldLog = defaultOutput stdout
      | otherwise = \_ _ _ _ -> pure ()

    action' = withSqliteConn (T.pack fp) action

-------------------------------------------------------------------------------
-- Migrate
-------------------------------------------------------------------------------

createTables :: MonadUnliftIO m => String -> m ()
createTables fp = do
  migrations <- withConnection' fp False $ runSqlConn $ runMigrationSilent migrateAll
  liftIO $ putStrLn "Executed migrations:"
  liftIO $ mapM T.putStr migrations
  liftIO $ print migrations

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
-- Query
-------------------------------------------------------------------------------

queryState :: (MonadIO m, MonadUnliftIO m) => SqlBackend -> m (Maybe State)
queryState = runSqlConn $ do
  states <- selectList [] []
  pure $ listToMaybe (entityVal <$> states)

-------------------------------------------------------------------------------
-- Delete
-------------------------------------------------------------------------------

-- | Returns True if deleted
deleteState :: (MonadIO m, MonadUnliftIO m) => SqlBackend -> m Bool
deleteState = runSqlConn $ do
  count <- deleteWhereCount @State []
  pure $ count > 0
