{-# LANGUAGE OverloadedStrings #-}

module AdaWallet (main) where

import Control.Monad
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Database.Sqlite
import Options.Applicative hiding (columns)
import System.Directory
import System.Environment
import Prelude

main :: IO ()
main = join $ execParser (info opts idm)

opts :: Parser (IO ())
opts =
  hsubparser
    ( command "wipe" (info (pure wipeCommand) (progDesc "wipe all state"))
        <> command "create" (info (pure createDatabase) (progDesc "create sqlite database"))
    )

wipeCommand :: IO ()
wipeCommand = do
  dir <- stateDir
  System.Directory.removeDirectoryRecursive dir

stateDir :: IO FilePath
stateDir = do
  fromEnv <- lookupEnv "ADAWALLET_STATE"
  let fromXdg = System.Directory.getXdgDirectory System.Directory.XdgData "adawallet"
  maybe fromXdg pure fromEnv

walletName :: IO String
walletName = do
  fromEnv <- lookupEnv "ADAWALLET_NAME"
  case fromEnv of
    Nothing -> pure "default"
    Just name -> pure name

createDatabase :: IO ()
createDatabase = do
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
