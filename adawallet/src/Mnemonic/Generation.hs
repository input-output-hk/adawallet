{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


module Mnemonic.Generation where

import Prelude 

import Cardano.Mnemonic
import Options.Applicative.MnemonicSize
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text 


-- | Generate a 9, 12, 15, 18, 21, or 24 word mnemonic (See Cardano.Mnemonic in cardano-address for more information).
-- Dictionary: https://github.com/cardano-foundation/cardano-wallet/blob/master/specifications/mnemonic/english.txt
createMnemonic :: MnemonicSize -> IO Text
createMnemonic size = do
  m <- case size of
         MS_9  -> mnemonicToText @9  . entropyToMnemonic <$> genEntropy
         MS_12 -> mnemonicToText @12 . entropyToMnemonic <$> genEntropy
         MS_15 -> mnemonicToText @15 . entropyToMnemonic <$> genEntropy
         MS_18 -> mnemonicToText @18 . entropyToMnemonic <$> genEntropy
         MS_21 -> mnemonicToText @21 . entropyToMnemonic <$> genEntropy
         MS_24 -> mnemonicToText @24 . entropyToMnemonic <$> genEntropy
  pure $ Text.unwords m