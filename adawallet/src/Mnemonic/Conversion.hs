{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- This module is concerned with converting a mnemonic to
-- cardano-cli compatible verification and signing keys
module Mnemonic.Conversion where

import Prelude

import Cardano.Address.Derivation
import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import Cardano.Mnemonic
import Codec.Binary.Bech32 hiding (encode)
import qualified Codec.Binary.Bech32 as Bech32
import Codec.Binary.Encoding
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except.Extra
import Control.Monad.Trans.Fail
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import GHC.TypeLits
import GHC.Word
import Options.Applicative.Derivation
import Options.Applicative.Style

data RootExtendedKeyGenerationError
  = MnemonicError (MkSomeMnemonicError '[24])
  | RootKeyGenerationError IOException

mnemonicToRootExtendedPrivateKey ::
  [Text] -> IO (Either RootExtendedKeyGenerationError ByteString)
mnemonicToRootExtendedPrivateKey mnemonic = runExceptT $ do
  -- TODO: Support password protected mnemonics
  -- TODO: Potentially support Byron, Icarus and Shared mnemonics
  -- TODO: Potentially support other sizes of mnemonics (9, 12, 15, 18, 21)
  let password = Nothing
      style = Shelley
  someMnemonic <-
    hoistEither . first MnemonicError $
      mkSomeMnemonic mnemonic
  rootK <-
    handleIOExceptT RootKeyGenerationError $
      generateRootKey someMnemonic password style
  return $ encode (EBech32 $ styleHrp style) (xprvToBytes rootK)

-- TODO: Export from cardano-address
styleHrp :: Style -> HumanReadablePart
styleHrp Shared = CIP5.root_shared_xsk
styleHrp _ = CIP5.root_xsk

data PrivateExtendedKeyError
  = DataPartDecodeError
  | Bech32DecodeError Bech32.DecodingError
  | DerivationPathError String
  | XPrvDecodeError ByteString
  | ChildKeyBech32HumuanReadablePartError String

-- | Convert the root extended private key to an extended private key.
-- https://github.com/uniVocity/cardano-tutorials/blob/master/cardano-addresses.md#understanding-the-hd-wallet-address-format-bip-44
rootExtendedPrivateKeyToPrivateKey :: ByteString -> Either PrivateExtendedKeyError ByteString
rootExtendedPrivateKeyToPrivateKey rootExtendedPrivateKeyBytes = do
  (hrp, dataPart) <-
    first Bech32DecodeError . Bech32.decodeLenient $
      Text.decodeUtf8 rootExtendedPrivateKeyBytes
  bytes <- maybe (Left DataPartDecodeError) Right $ Bech32.dataPartToBytes dataPart
  xprv <- maybe (Left $ XPrvDecodeError bytes) Right $ xprvFromBytes bytes
  let defaultDerivationPath = "1852H/1815H/0H/0/0"
  derivPath <- first DerivationPathError $ derivationPathFromString defaultDerivationPath

  let ixs = castDerivationPath derivPath
      scheme =
        if length ixs == 2 && hrp == CIP5.root_xsk
          then DerivationScheme1
          else DerivationScheme2
  let Identity child = foldM (\k -> pure . deriveXPrv scheme k) xprv ixs
  (hrp, child) <-
    (,xprvToBytes child)
      <$> first ChildKeyBech32HumuanReadablePartError (runFail $ childHrpFor (indexToWord32 <$> ixs) hrp)
  return $ encode (EBech32 hrp) child
  where
    -- NB: We wholesale copy this from cardano-address
    -- As a reminder, we really have two scenarios:
    --
    -- Byron Legacy:
    --
    --     m / rnd_account' / rnd_address'
    --
    --
    -- Icarus & Shelley:
    --
    --     m / purpose' / coin_type' / account' / role / index
    --
    -- We do not allow derivations to anywhere in the path to avoid people
    -- shooting themselves in the foot.
    -- Hence We only allow the following transformations:
    --
    -- root_xsk => addr_xsk: (legacy)
    --     m => m / rnd_account' / rnd_address
    --
    -- root_xsk => acct_xsk: (hard derivation from root to account)
    --     m => m / purpose' / coin_type' / account'
    --
    -- root_xsk => acct_xsk: (hard derivation from root to address)
    --     m => m / purpose' / coin_type' / account' / role / index
    --
    -- purpose' = 1852H for shelley wallet addresses.
    -- purpose' = 1854H for shelley wallet addresses that expose shared account.
    --
    -- acct_xsk => addr_xsk: (hard derivation from account to address)
    --     m / purpose' / coin_type' / account' => m / purpose' / coin_type' / account' / role / index
    --
    -- acct_xvk => addr_xvk: (soft derivation from account to address)
    --     m / purpose' / coin_type' / account' => m / purpose' / coin_type' / account' / role / index
    --
    --

    -- Shared:
    --
    --     m / purpose' / coin_type' / account' / role / index
    --
    -- purpose' = 1854H for shared wallet addresses.
    --
    -- As with Icarus/Shelley sequential wallets, to prevent undiscoverable
    -- addresses, we allow only the following transformations:
    --
    -- shared_root_xsk => shared_acct_xsk: (hard derivation from root to account)
    --     m => m / purpose' / coin_type' / account'
    --
    -- shared_root_xsk => shared_acct_xsk: (hard derivation from root to address)
    --     m => m / purpose' / coin_type' / account' / role / index
    --
    -- shared_acct_xsk => shared_addr_xsk: (hard derivation from account to address)
    --     m / purpose' / coin_type' / account' => m / purpose' / coin_type' / account' / role / index
    --
    -- shared_acct_xvk => shared_addr_xvk: (soft derivation from account to address)
    --     m / purpose' / coin_type' / account' => m / purpose' / coin_type' / account' / role / index
    --
    --
    -- There's no use-case at the moment for accessing intermediate paths such
    -- as m / purpose' or m / purpose' / coin_type' so we do not expose them.
    childHrpFor :: [Word32] -> HumanReadablePart -> Fail String HumanReadablePart
    childHrpFor [_, _, _, 2, _] hrp
      | hrp == CIP5.root_xsk = pure CIP5.stake_xsk
      | hrp == CIP5.root_shared_xsk = pure CIP5.stake_shared_xsk
    childHrpFor [p, _, _, _, _] hrp
      | hrp == CIP5.root_xsk =
          -- 2147485502 stands for 1854H
          if p == 2147485502
            then pure CIP5.addr_shared_xsk
            else pure CIP5.addr_xsk
      | hrp == CIP5.root_shared_xsk = pure CIP5.addr_shared_xsk
    childHrpFor [p, _, _] hrp
      | hrp == CIP5.root_xsk =
          -- 2147485502 stands for 1854H
          if p == 2147485502
            then pure CIP5.acct_shared_xsk
            else -- 2147485503 stands for 1855H

              if p == 2147485503
                then pure CIP5.policy_xsk
                else pure CIP5.acct_xsk
      | hrp == CIP5.root_shared_xsk = pure CIP5.acct_shared_xsk
    childHrpFor [2, _] hrp
      | hrp == CIP5.acct_xsk = pure CIP5.stake_xsk
      | hrp == CIP5.acct_xvk = pure CIP5.stake_xvk
      | hrp == CIP5.acct_shared_xsk = pure CIP5.stake_shared_xsk
      | hrp == CIP5.acct_shared_xvk = pure CIP5.stake_shared_xvk
    childHrpFor [_, _] hrp
      | hrp == CIP5.root_xsk = pure CIP5.addr_xsk
      | hrp == CIP5.acct_xsk = pure CIP5.addr_xsk
      | hrp == CIP5.acct_xvk = pure CIP5.addr_xvk
      | hrp == CIP5.acct_shared_xsk = pure CIP5.addr_shared_xsk
      | hrp == CIP5.acct_shared_xvk = pure CIP5.addr_shared_xvk
    childHrpFor _ hrp
      | hrp == CIP5.root_xsk =
          fail
            "When deriving child keys from a parent root key, you must \
            \provide either 2, 3 or 5 path segments. Provide 2 (account and \
            \address) if you intend to derive a legacy Byron key. Provide 3 or 5 \
            \(purpose, coin_type, account, role, index) if you're dealing with \
            \anything else."
      | hrp == CIP5.root_shared_xsk =
          fail
            "When deriving child keys from a parent root key, you must \
            \provide either 3 or 5 path segments. Provide 3 \
            \(purpose, coin_type, account) or 5 \
            \(purpose, coin_type, account, role, index)."
      | otherwise =
          fail
            "When deriving child keys from a parent account key, you must \
            \provide exactly two path segments (role & index)."

extendedPrivateKeyToCardanoCliPrivateKey = error "Use cardano-cli's readSomeCardanoAddressSigningKeyFile"
