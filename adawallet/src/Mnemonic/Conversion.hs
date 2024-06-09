{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
This module is concerned with converting a mnemonic to 
cardano-cli compatible verification and signing keys
-}
module Mnemonic.Conversion where 

import Prelude 

import Cardano.Address.Derivation
import Cardano.Mnemonic
import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import Codec.Binary.Bech32 hiding (encode)
import Codec.Binary.Encoding
import Control.Exception
import Control.Monad.Trans.Except.Extra
import Data.Bifunctor
import Data.Text (Text)
import Data.ByteString (ByteString)
import GHC.TypeLits
import Options.Applicative.Style

data RootExtendedKeyGenerationError
  = MnemonicError (MkSomeMnemonicError '[24])
  | RootKeyGenerationError IOException

nemonicToRootExtendedPrivateKey 
  :: [Text] -> IO (Either RootExtendedKeyGenerationError ByteString) 
nemonicToRootExtendedPrivateKey mnemonic = runExceptT $ do 
  -- TODO: Support password protected mnemonics 
  -- TODO: Potentially support Byron, Icarus and Shared mnemonics
  -- TODO: Potentially support other sizes of mnemonics (9, 12, 15, 18, 21)
  let password = Nothing 
      style = Shelley
  someMnemonic <- hoistEither . first MnemonicError 
                    $ mkSomeMnemonic mnemonic
  rootK <- handleIOExceptT RootKeyGenerationError  
             $ generateRootKey someMnemonic password style
  return $ encode (EBech32 $ styleHrp style) (xprvToBytes rootK)

-- TODO: Export from cardano-address
styleHrp :: Style -> HumanReadablePart
styleHrp Shared = CIP5.root_shared_xsk
styleHrp _ = CIP5.root_xsk


