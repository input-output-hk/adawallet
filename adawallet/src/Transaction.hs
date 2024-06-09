{-# LANGUAGE ScopedTypeVariables #-}

module Transaction where

import qualified Blockfrost.Client as BF
import Cardano.Api as Api
import Cardano.Api.Shelley
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.Foldable as Fld
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding as Text
import GHC.Stack
import Money
import Numeric (readHex)
import Text.Pretty.Simple
import Prelude

readBfTx :: HasCallStack => IO ()
readBfTx = do
  -- a token for preview network
  -- BLOCKFROST_TOKEN_PATH="$PATH/adawallet/blockfrost.preview.token"
  prj <- BF.projectFromEnv
  let address = "addr_test1wqh4yha0ndhwykrh9cuhr47nh2y97zvkls74h4jq6uhlpacujv3z3"
      -- TODO figure this out dynamically
      era = AlonzoEraOnwardsBabbage
  (latestBlocks, utxos) <- fmap (either (error . show) id) . BF.runBlockfrost prj $ do
    latestBlocks <- BF.getLatestBlock
    utxos <- BF.getAddressUtxos address
    pure (latestBlocks, utxos)

  pPrint latestBlocks
  pPrint utxos
  let apiTxs = toApiTxOut era address <$> utxos
  pPrint apiTxs

toApiUTxO ::
  HasCallStack =>
  AlonzoEraOnwards era ->
  BF.Address ->
  [BF.AddressUtxo] ->
  UTxO era
toApiUTxO w addr =
  Fld.foldl'
    ( \(UTxO acc) addrUTxO -> UTxO $ acc <> Map.singleton (toApiTxIn addrUTxO) (toApiTxOut w addr addrUTxO)
    )
    (UTxO mempty)

toApiTxIn :: BF.AddressUtxo -> TxIn
toApiTxIn adUTxO =
  TxIn
    (fromString . Text.unpack . BF.unTxHash $ BF._addressUtxoTxHash adUTxO)
    (TxIx . fromIntegral $ BF._addressUtxoOutputIndex adUTxO)

toApiTxOut ::
  forall era.
  HasCallStack =>
  AlonzoEraOnwards era ->
  BF.Address ->
  BF.AddressUtxo ->
  TxOut CtxUTxO era
toApiTxOut w (BF.Address addr) addrUTxO =
  case deserialiseAddress (AsAddress AsShelleyAddr) addr of
    Nothing -> error $ "toApiTxOut: Shelley address deserialization failed: " <> Text.unpack addr
    Just bech32Addr ->
      let addrInEra = AddressInEra (ShelleyAddressInEra (alonzoEraOnwardsToShelleyBasedEra w)) bech32Addr
          val =
            toLedgerValue (alonzoEraOnwardsToMaryEraOnwards w) $
              Fld.foldl' (\acc amt -> acc <> toApiValue amt) mempty $
                BF._addressUtxoAmount addrUTxO
          mDatHash = BF._addressUtxoDataHash addrUTxO
          mInlineDat = BF._addressUtxoInlineDatum addrUTxO
          sbe = alonzoEraOnwardsToShelleyBasedEra w
          v = shelleyBasedEraConstraints sbe $ TxOutValueShelleyBased sbe val :: TxOutValue era
       in -- NB: Blockfrost only returns the reference script hash
          TxOut addrInEra v (toApiDatum w mDatHash mInlineDat) ReferenceScriptNone

toApiDatum ::
  HasCallStack =>
  AlonzoEraOnwards era ->
  Maybe BF.DatumHash ->
  Maybe BF.InlineDatum ->
  TxOutDatum CtxUTxO era
toApiDatum w mDatumHash mInlineDatum = do
  let era = alonzoEraOnwardsToCardanoEra w
  case (mDatumHash, mInlineDatum) of
    (Nothing, Nothing) -> TxOutDatumNone
    (Just (BF.DatumHash dHash), Nothing) ->
      TxOutDatumHash w $ fromString $ Text.unpack dHash
    (_, Just (BF.InlineDatum (BF.ScriptDatumCBOR sCbor))) -> either error id $ do
      let bytes = hexToByteString sCbor
      sData <-
        first (("toApiDatum: Error deserializing inline datum: " <>) . show) $
          deserialiseFromCBOR AsScriptData bytes
      let hScriptData = unsafeHashableScriptData sData
          hash = hashScriptDataBytes hScriptData
      maybe (Left $ "Impossible case - unsupported era: " <> show (pretty era)) pure
        . getFirst
        . mconcat
        . map First
        $ [ forEraInEonMaybe era $ \w ->
              TxOutDatumInline w hScriptData
          , forEraInEonMaybe era $ \w ->
              TxOutDatumHash w hash
          , Just TxOutDatumNone
          ]

toApiValue :: HasCallStack => BF.Amount -> Api.Value
toApiValue (BF.AdaAmount ll) = lovelaceToValue . fromInteger $ toInteger ll
toApiValue (BF.AssetAmount a) = toApiMultiAsset a

hexToByteString :: HasCallStack => T.Text -> BS.ByteString
hexToByteString txt = BS.pack . map (fst . head . readHex . T.unpack) $ T.chunksOf 2 txt

toApiMultiAsset :: HasCallStack => SomeDiscrete -> Api.Value
toApiMultiAsset sDiscrete =
  let aId = someDiscreteCurrency sDiscrete
      q = someDiscreteAmount sDiscrete
      (pId, aName) = Text.splitAt 56 aId
      assetName =
        either
          (error $ "toApiMultiAsset: " <> Text.unpack aName <> " is not hex encoded")
          id
          (deserialiseFromRawBytesHex AsAssetName $ Text.encodeUtf8 aName)
   in valueFromList
        [
          ( AssetId (fromString $ Text.unpack pId) assetName
          , fromInteger q
          )
        ]
