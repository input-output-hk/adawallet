{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module AdaWallet (main) where

import Cardano.Address.Derivation (Depth (RootK), XPrv, genMasterKeyFromMnemonic)
import Cardano.Address.Style.Shelley (Shelley)
import Cardano.Mnemonic (
  entropyToMnemonic,
  genEntropy,
  getMkSomeMnemonicError,
  mkSomeMnemonic,
  mnemonicToText,
 )
import Cardano.Prelude (ExceptT, runExceptT)
import Control.Exception (throwIO)
import Control.Monad (forM_, join)
import Control.Monad.Trans.Except.Extra (left)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Database.Persist.Sqlite (PersistValue (PersistText), toPersistValue)
import Database.Sqlite (Connection, bind, close, finalize, open, prepare, stepConn)
import Options.Applicative (Parser, command, execParser, hsubparser, idm, info, progDesc)
import System.Directory (
  XdgDirectory (..),
  createDirectoryIfMissing,
  getXdgDirectory,
  removePathForcibly,
 )
import System.Environment (lookupEnv)
import Prelude

main :: IO ()
main = do
  initialize
  join $ execParser (info opts idm)

opts :: Parser (IO ())
opts =
  hsubparser
    ( command "wipe" (info (pure wipeCommand) (progDesc "wipe all state"))
        --        <> command "init-restore" (info (pure restoreWallet "foo") (progDesc "Restore a wallet from mnemonic"))
        <> command "init-create" (info (pure createWallet) (progDesc "create a wallet"))
        --        <> command "import-accounts" (info (pure importAccounts 0 0) (progDesc "create a wallet"))
    )

wipeCommand :: IO ()
wipeCommand = do
  stateDir' <- stateDir
  walletName' <- walletName
  let sqliteFile = stateDir' ++ "/" ++ walletName' ++ ".sqlite"
  removePathForcibly sqliteFile

stateDir :: IO FilePath
stateDir = do
  fromEnv <- lookupEnv "ADAWALLET_STATE"
  let fromXdg = getXdgDirectory XdgData "adawallet"
  maybe fromXdg pure fromEnv

walletName :: IO String
walletName = do
  fromEnv <- lookupEnv "ADAWALLET_NAME"
  case fromEnv of
    Nothing -> pure "default"
    Just name -> pure name

-- TODO: do more than just create tables
initialize :: IO ()
initialize = do
  conn <- openDB
  let queries =
        map
          (prepare conn)
          [ "CREATE TABLE IF NOT EXISTS status(hw_wallet,root_key,testnet,blockfrost_url);"
          , "CREATE TABLE IF NOT EXISTS utxo(txid,tx_index,address,amount);"
          , "CREATE TABLE IF NOT EXISTS accounts(id,payment_vkey,payment_skey,stake_vkey,stake_skey,address,stake_address);"
          ]
  forM_ queries (>>= stepConn conn)

-- Restores a wallet from a mnemonic and loads the private key into sqlite
restoreWallet :: String -> IO ()
restoreWallet mmemonic = putStrLn "Not implemented yet"

openDB :: IO Database.Sqlite.Connection
openDB = do
  stateDir' <- stateDir
  createDirectoryIfMissing True stateDir'
  walletName' <- walletName
  let sqliteFile = stateDir' ++ "/" ++ walletName' ++ ".sqlite"
  open $ T.pack sqliteFile

-- Creates a new wallet, prints to stdout and loads the private key into sqlite
createWallet :: IO ()
createWallet = do
  mnemonic <- mnemonicToText @24 . entropyToMnemonic <$> genEntropy
  TI.putStrLn $ T.unwords mnemonic
  rootKey <- runExceptT $ mnemonicToRootKey mnemonic
  case rootKey of
    Left _ -> return ()
    Right rootKey' -> do
      conn <- openDB
      statement <- prepare conn "INSERT INTO status (hw_wallet,root_key) VALUES (False,?,False,\"\")"

      bind statement [PersistText rootKey']
      stepConn conn statement
      finalize statement
      close conn
      return ()

newtype FaucetError = FaucetErrorBadMnemonic T.Text

mnemonicToRootKey :: Monad m => [T.Text] -> ExceptT FaucetError m (Shelley 'RootK XPrv)
mnemonicToRootKey mnemonic = do
  mw <-
    either (left . FaucetErrorBadMnemonic . T.pack . getMkSomeMnemonicError) pure $
      mkSomeMnemonic @'[24] mnemonic
  pure $ genMasterKeyFromMnemonic mw mempty

-- Imports accounts for an already restored wallet (0th index for payment/stake/drep/CC cold/CC hot)
importAccounts :: Int -> Int -> IO ()
importAccounts start end = putStrLn "Not implemented yet"

-- Signs transaction
-- TODO replace [String] with list of custom types to sign with
signTx :: FilePath -> Int -> [String] -> IO ()
signTx fp account types = putStrLn "Not implemented yet"

-- Signs multiple transactions in a tarball
-- TODO replace [String] with list of custom types to sign with
bulkSignTx :: FilePath -> Int -> [String] -> IO ()
bulkSignTx fp account types = putStrLn "Not implemented yet"
