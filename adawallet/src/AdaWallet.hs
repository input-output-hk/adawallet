{-# LANGUAGE OverloadedStrings #-}

module AdaWallet (main) where

import Control.Monad (forM_, join)
import Data.Foldable (traverse_)
import Data.Maybe
import qualified Data.Text as T
import Database.Sqlite (open, prepare, stepConn)
import GHC.Stack
import Options.Applicative (Parser, command, execParser, hsubparser, idm, info, progDesc)
import System.Directory (
  XdgDirectory (..),
  createDirectoryIfMissing,
  getXdgDirectory,
  removePathForcibly,
 )
import System.Environment (lookupEnv)
import Transaction
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
        <> command "debug-rtx" (info (pure readTx) (progDesc "read tx from blockfrost and print"))
    )

wipeCommand :: HasCallStack => IO ()
wipeCommand = do
  stateDir' <- stateDir
  walletName' <- walletName
  let sqliteFile = stateDir' ++ "/" ++ walletName' ++ ".sqlite"
  removePathForcibly sqliteFile

stateDir :: HasCallStack => IO FilePath
stateDir = do
  fromEnv <- lookupEnv "ADAWALLET_STATE"
  let fromXdg = getXdgDirectory XdgData "adawallet"
  maybe fromXdg pure fromEnv

walletName :: IO String
walletName = fromMaybe "default" <$> lookupEnv "ADAWALLET_NAME"

-- TODO: do more than just create tables
initialize :: HasCallStack => IO ()
initialize = do
  stateDir' <- stateDir
  createDirectoryIfMissing True stateDir'
  walletName' <- walletName
  let sqliteFile = stateDir' ++ "/" ++ walletName' ++ ".sqlite"
  conn <- open $ T.pack sqliteFile

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
restoreWallet mmemonic = error "Not implemented yet"

-- Creates a new wallet, prints to stdout and loads the private key into sqlite
createWallet :: HasCallStack => IO ()
createWallet = error "Not implemented yet"

-- Restores a wallet from an exported json file comtaining account data with no secrets
restoreWalletReadOnly :: HasCallStack => FilePath -> IO ()
restoreWalletReadOnly json_file = error "Not implemented yet"

-- Exports accounts to json without secret keys
exportAccountsNoSecrets :: HasCallStack => FilePath -> IO ()
exportAccountsNoSecrets json_file = error "Not implemented yet"

-- Imports accounts for an already restored wallet (0th index for payment/stake/drep/CC cold/CC hot)
importAccounts :: HasCallStack => Int -> Int -> IO ()
importAccounts start end = error "Not implemented yet"

-- Signs transaction
-- TODO replace [String] with list of custom types to sign with
signTx :: HasCallStack => FilePath -> Int -> [String] -> IO ()
signTx fp account types = error "Not implemented yet"

-- Signs multiple transactions in a tarball
-- TODO replace [String] with list of custom types to sign with
bulkSignTx :: HasCallStack => FilePath -> Int -> [String] -> IO ()
bulkSignTx fp account types = error "Not implemented yet"

readTx :: HasCallStack => IO ()
readTx = readBfTx
