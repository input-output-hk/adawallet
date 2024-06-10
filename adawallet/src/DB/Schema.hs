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
import Data.Text (Text)
import Data.ByteString (ByteString)
import Database.Persist.TH
import Prelude
import Data.ByteString

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

    Utxo
      txid          ByteString
      tx_index      ByteString
      address       ByteString
      amount        ByteString

    Account
      idx               Int
      vkey              Text
      name              Text
      Primary idx
   |]