{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module AdaWallet (main) where

import Cardano.Mnemonic (
  MkSomeMnemonic (..),
  SomeMnemonic,
  entropyToMnemonic,
  genEntropy,
  mkMnemonic,
  mnemonicToText,
 )
import Control.Exception (throwIO)
import Control.Monad (forM_, join)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Database.Sqlite (open, prepare, stepConn)
import GHC.Stack
import Mnemonic.Conversion (mnemonicToRootExtendedPrivateKey)
import Mnemonic.Generation
import Mnemonic.Generation (createMnemonic)
import Options.Applicative (
  Parser,
  command,
  execParser,
  helper,
  hsubparser,
  idm,
  info,
  progDesc,
  (<**>),
 )
import Options.Applicative.MnemonicSize (MnemonicSize (..))
import Options.Applicative.Style (
  PassphraseInfo (..),
  PassphraseInput (..),
  PassphraseInputMode (..),
 )
import System.Directory (
  XdgDirectory (..),
  createDirectoryIfMissing,
  getXdgDirectory,
  removePathForcibly,
 )
import System.Environment (lookupEnv)
import System.IO
import System.IO.Extra (hGetPassphraseBytes, hGetSomeMnemonicInteractively)
import Transaction
import Prelude

data WalletState = WalletState
  { rootXprv :: ByteString
  , accounts :: [(ByteString, ByteString, ByteString, ByteString, ByteString)]
  , blockFrostProjectId :: String
  , isTestnet :: Bool
  }

-- TODO query from database
queryWalletState :: IO WalletState
queryWalletState = do
  bfProjectId <- queryBFProjectId
  pure $ WalletState "" [] bfProjectId True

main :: IO ()
main = do
  initialize
  walletState <- queryWalletState
  join $ execParser $ info (opts walletState <**> helper) idm

opts :: WalletState -> Parser (IO ())
opts walletState =
  hsubparser
    ( command "wipe" (info (pure wipeCommand) (progDesc "wipe all state"))
        --        <> command "init-restore" (info (pure restoreWallet "foo") (progDesc "Restore a wallet from mnemonic"))
        <> command "init-create" (info (pure (createWallet defaultArgs)) (progDesc "create a wallet"))
        --        <> command "import-accounts" (info (pure importAccounts 0 0) (progDesc "create a wallet"))
        <> command
          "debug-rtx"
          (info (pure $ readTx walletState) (progDesc "read tx from blockfrost and print"))
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

-- TODO get project ID from sql
queryBFProjectId :: HasCallStack => IO String
queryBFProjectId = do
  envVar <- lookupEnv "ADAWALLET_BLOCKFROST_PROJ_ID"
  case envVar of
    Nothing -> error "no blockfrost project id set"
    Just projid -> pure projid

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

data MnemonicSource
  = StdInput
  | Generate

newtype Arg = Arg
  { source :: MnemonicSource -- Nothing to create
  }

defaultArgs :: Arg
defaultArgs = Arg StdInput

-- Creates a new wallet, prints to stdout and loads the private key into sqlite
createWallet :: HasCallStack => Arg -> IO ()
createWallet arg = do
  someMnemonic <- case source arg of
    StdInput -> do
      hGetSomeMnemonicInteractively (stdin, stderr) Explicit prompt
    Generate -> do
      mnemonic <- createMnemonic MS_24
      case mkSomeMnemonic @'[24] mnemonic of
        Left err -> error $ show err
        Right a -> pure a
  passphrase <-
    hGetPassphraseBytes (stdin, stderr) Explicit Interactive promptPass Utf8
  insertMnemonicPassword someMnemonic passphrase
  where
    prompt = "Please enter a [9, 12, 15, 18, 21, 24] word mnemonic:"
    promptPass = "Enter passphrase (empty for no passphrase):"

insertMnemonicPassword :: SomeMnemonic -> ByteString -> IO ()
insertMnemonicPassword = error "unimplemented"

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

readTx :: HasCallStack => WalletState -> IO ()
readTx walletState = do
  let bfProjId = blockFrostProjectId walletState
  readBfTx $ Text.pack bfProjId
