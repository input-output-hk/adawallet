{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module AdaWallet (main) where

import Cardano.Api
import Cardano.Mnemonic (
  MkSomeMnemonic (..),
  SomeMnemonic (..),
  entropyToMnemonic,
  genEntropy,
  mkMnemonic,
  mnemonicToText,
 )
import qualified Codec.Binary.Bech32 as Bech32
import Control.Exception (throwIO)
import Control.Monad (forM_, join, void)
import qualified DB.Schema as DB
import Data.Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Char8 (unpack)
import Data.Foldable (traverse_)
import Data.Maybe
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Database
import Database.Sqlite (finalize, open, prepare, stepConn)
import GHC.Stack
import Mnemonic.Conversion (mnemonicToRootExtendedPrivateKey)
import Mnemonic.Generation (createMnemonic)
import Options.Applicative (
  Parser,
  argument,
  command,
  execParser,
  flag,
  fullDesc,
  header,
  help,
  helper,
  hsubparser,
  idm,
  info,
  long,
  progDesc,
  short,
  str,
  strOption,
  switch,
  (<**>),
 )
import Options.Applicative.MnemonicSize (MnemonicSize (..))
import Options.Applicative.Style (
  Passphrase (..),
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
import System.IO.Extra (
  hGetPassphraseBytes,
  hGetSomeMnemonicInteractively,
 )
import Text.Pretty.Simple (pPrint)
import Transaction
import Prelude

-- TODO figure this out dynamically
currentEra :: ShelleyBasedEra BabbageEra
currentEra = ShelleyBasedEraBabbage

-- walletState passed to all commands that need it
data Account = Account
  { accountIndex :: Int
  , accountSkey :: Maybe ByteString
  , accountVkey :: ByteString
  , paymentSkey :: Maybe ByteString
  , paymentVkey :: ByteString
  , stakeSkey :: Maybe ByteString
  , stakeVkey :: ByteString
  , drepSkey :: Maybe ByteString
  , drepVkey :: ByteString
  , ccColdSkey :: Maybe ByteString
  , ccColdVkey :: ByteString
  , ccHotSkey :: Maybe ByteString
  , ccHotVkey :: ByteString
  , address :: String
  , stakeAddress :: String
  }
  deriving (Show, Read, Eq)

data WalletState = WalletState
  { rootXprv :: ByteString
  , rootXprvEncrypted :: Bool
  , accounts :: [Account]
  , blockFrostProjectId :: String
  , isTestnet :: Bool
  }
  deriving (Show, Read, Eq)

-- sqlite table structures
data StateTable = StateTable
  { version :: Int
  , root_key :: ByteString
  , is_encrypted :: Bool
  , is_testnet :: Bool
  , blockfrost_project_id :: String
  }
  deriving (Show, Read, Eq)

data AccountTable = AccountTable
  { idx :: Int
  , vkey :: ByteString
  , name :: Maybe String
  }
  deriving (Show, Read, Eq)

-- TODO query from database
queryWalletState :: IO WalletState
queryWalletState = do
  bfProjectId <- queryBFProjectId
  rootKey <- queryRootKey

  let acct =
        Account
          0
          Nothing
          "acct_xvk1jk900g6aj69l6yx24ug7cdc2ax7cxesrg2synxaevj2j9mlmtysp9jc6j3pezn7y9zkegfdv0lfyln9wdw7zsu64wj0l04k99ugktlgvnx703"
          Nothing
          ""
          Nothing
          ""
          Nothing
          ""
          Nothing
          ""
          Nothing
          ""
          ""
          ""
      accounts = [acct]

  pure $ WalletState rootKey False accounts bfProjectId True

main :: IO ()
main = do
  initialize
  cmd <-
    execParser $
      info
        (opts <**> helper)
        ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "hello - a test for optparse-applicative"
        )
  case cmd of
    CommandWipe -> wipeCommand
    CommandCreateWallet options -> createWallet options
    CommandDebugRTX -> do
      walletState <- queryWalletState
      readTx walletState
    CommandDebugWalletState -> do
      walletState <- queryWalletState
      debugWalletState walletState
    CommandSign -> signTx undefined undefined undefined

data Command
  = CommandWipe
  | CommandCreateWallet CreateWalletOptions
  | CommandDebugRTX
  | CommandDebugWalletState
  | CommandSign

opts :: Parser Command
opts =
  hsubparser
    ( command "wipe" (info (pure CommandWipe) (progDesc "wipe all state"))
        --        <> command "init-restore" (info (pure restoreWallet "foo") (progDesc "Restore a wallet from mnemonic"))
        <> command
          "init-create"
          (info (CommandCreateWallet <$> createWalletOptions) (progDesc "create a wallet"))
        --        <> command "import-accounts" (info (pure importAccounts 0 0) (progDesc "create a wallet"))
        <> command
          "debug-rtx"
          (info (pure CommandDebugRTX) (progDesc "read tx from blockfrost and print"))
        <> command
          "debug-wallet-state"
          (info (pure CommandDebugWalletState) (progDesc "debug wallet state"))
        <> command "sign" (info (pure CommandSign) (progDesc "sign a tx"))
    )

sqliteFilePath :: HasCallStack => IO FilePath
sqliteFilePath = do
  stateDir' <- stateDir
  walletName' <- walletName
  pure $ stateDir' ++ "/" ++ walletName' ++ ".sqlite"

wipeCommand :: HasCallStack => IO ()
wipeCommand = do
  sqliteFile <- sqliteFilePath
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

-- TODO get root key from sql
queryRootKey :: HasCallStack => IO ByteString
queryRootKey = do
  envVar <- lookupEnv "ADAWALLET_ROOT_KEY"
  case envVar of
    Nothing -> error "no root key set"
    Just rootKey -> pure $ fromString rootKey

walletName :: IO String
walletName = fromMaybe "default" <$> lookupEnv "ADAWALLET_NAME"

-- TODO: do more than just create tables
initialize :: HasCallStack => IO ()
initialize = do
  stateDir' <- stateDir
  createDirectoryIfMissing True stateDir'
  walletName' <- walletName
  sqliteFile <- sqliteFilePath
  conn <- open $ T.pack sqliteFile

  let queries =
        map
          (prepare conn)
          [ "CREATE TABLE IF NOT EXISTS state(id INT PRIMARY KEY,version INT,root_key BLOB,is_encrypted INT,is_testnet INT,blockfrost_project_id TEXT) STRICT;"
          , "CREATE TABLE IF NOT EXISTS account(id INT PRIMARY KEY,idx INTEGER UNIQUE,vkey BLOB NOT NULL,name TEXT) STRICT;"
          , -- Not used yet
            "CREATE TABLE IF NOT EXISTS utxo(id INT PRIMARY KEY,txid,tx_index,address,amount);"
          ]
  forM_ queries (>>= stepConn conn)
  forM_ queries (>>= finalize)

-- Restores a wallet from a mnemonic and loads the private key into sqlite
restoreWallet :: String -> IO ()
restoreWallet mmemonic = error "Not implemented yet"

data MnemonicSource
  = StdInput
  | Generate

data CreateWalletOptions = CreateWalletOptions
  { generate :: MnemonicSource
  , isTestnetA :: Bool
  , blockFrostProjectIdA :: String
  }

createWalletOptions :: Parser CreateWalletOptions
createWalletOptions =
  CreateWalletOptions
    <$> flag
      StdInput
      Generate
      ( long "generate"
          <> short 'g'
          <> help "Enable Mnemonic generation"
      )
    <*> switch
      ( long "testnet"
          <> short 't'
          <> help "Specify if testnet environment"
      )
    <*> strOption
      ( long "blockfrost-project-id"
          <> short 'b'
          <> help "Blockfrost Project ID"
      )

-- Creates a new wallet, prints to stdout and loads the private key into sqlite
createWallet :: HasCallStack => CreateWalletOptions -> IO ()
createWallet source = do
  someMnemonic <- case generate source of
    StdInput -> do
      hGetSomeMnemonicInteractively (stdin, stderr) Explicit prompt
    Generate -> do
      mnemonic <- createMnemonic MS_24
      case mkSomeMnemonic @'[24] mnemonic of
        Left err -> error $ show err
        Right v@(SomeMnemonic a) -> do
          let words = mnemonicToText a
              wordsString = unwords (map T.unpack words)
          print $ "Your mnemonic is: " ++ wordsString
          pure v
  let projectId = blockFrostProjectIdA source
      testnet = isTestnetA source
  passphrase <-
    hGetPassphraseBytes (stdin, stderr) Explicit Interactive promptPass Utf8

  putStrLn $ unpack passphrase

  root_key <- mnemonicToRootExtendedPrivateKey someMnemonic (maybePassphrase passphrase)
  case root_key of
    Left e -> error $ "problem occurred: " ++ show e
    Right xprv -> do
      let isEncrypted = passphrase /= ""
          stateTable = DB.State 1 xprv isEncrypted testnet (T.pack projectId)
      fp <- sqliteFilePath
      void $ withConnectionDebug fp (insertState stateTable)
  where
    maybePassphrase :: ByteString -> Maybe Passphrase
    maybePassphrase "" = Nothing
    maybePassphrase p = Just $ FromEncoded p

    prompt = "Please enter a [9, 12, 15, 18, 21, 24] word mnemonic:"
    promptPass = "Enter passphrase (empty for no passphrase):"

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
signTx fp account types = do
  pPrint
    =<< signTransaction currentEra signingKeyB16 txBodyJson
  where
    txBodyJson =
      "{ \"type\": \"Unwitnessed Tx BabbageEra\", \"description\": \"Ledger Cddl Format\", \"cborHex\": \"84a30081825820d87452ce6aa5e99228528dab29da613dbbba502db26fb67efe2d5c228f14e76e00018182581d607f1e7aa1a1d9a17cbb2f466d177f67b5b8e7caa2f31b87716e8313e418640200a0f5f6\" }"
    --  Signing key in Base16
    signingKeyB16 = "582037ef1428378d2b353096b419d425a37d62aec6c08d1162dcb129e4da73976e8b"

-- Signs multiple transactions in a tarball
-- TODO replace [String] with list of custom types to sign with
bulkSignTx :: HasCallStack => FilePath -> Int -> [String] -> IO ()
bulkSignTx fp account types = error "Not implemented yet"

readTx :: HasCallStack => WalletState -> IO ()
readTx walletState = do
  let bfProjId = fromString $ blockFrostProjectId walletState
  -- TODO remove hardcoded address
  let address = "addr_test1wqh4yha0ndhwykrh9cuhr47nh2y97zvkls74h4jq6uhlpacujv3z3"
  pPrint
    =<< readBlockfrostTransaction currentEra bfProjId address

debugWalletState :: HasCallStack => WalletState -> IO ()
debugWalletState walletState = do
  print walletState
