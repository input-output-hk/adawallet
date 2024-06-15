{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DB.Schema where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Persist.TH
import Prelude

share
  [ mkPersist sqlSettings
  , mkMigrate "migrateAll"
  , mkEntityDefList "entityDefs"
  ]
  [persistLowerCase|

    State
      version        Int
      root_key       ByteString
      is_encrypted   Bool
      is_testnet     Bool
      blockfrost_project_id  Text
      deriving Show

    Utxo
      txid          ByteString
      tx_index      Int
      address       ByteString
      amount        Int
      deriving Show

    Account
      idx               Int
      vkey              ByteString
      name              Text
      Primary idx
      deriving Show
   |]
