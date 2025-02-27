{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transaction where

import qualified Blockfrost.Auth as BF
import qualified Blockfrost.Client as BF
import Cardano.Api as Api
import Cardano.Api.SerialiseLedgerCddl as Api
import Cardano.Api.Shelley
import Cardano.CLI.Read (CddlTx (..))
import qualified Codec.Binary.Bech32 as Bech32
import Data.Aeson as A
import Data.Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import Data.Either
import Data.Foldable (traverse_)
import qualified Data.Foldable as Fld
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Type.Equality
import GHC.Exts (fromList)
import GHC.Stack
import Money
import Numeric (readHex)
import Text.Pretty.Simple
import Prelude

-- | Get blockfrost transactions for an address
readBlockfrostTransaction ::
  HasCallStack =>
  ShelleyBasedEra era ->
  -- | Blockfrost project token
  Text ->
  -- | Cardano address
  BF.Address ->
  IO [TxOut CtxUTxO era]
readBlockfrostTransaction sbe projid address = forShelleyBasedEraInEon sbe (error "Eras prior to Alonzo are not supported") $ \era -> do
  let prj = BF.mkProject projid
  (latestBlocks, utxos) <- fmap (either (error . show) id) . BF.runBlockfrost prj $ do
    latestBlocks <- BF.getLatestBlock
    utxos <- BF.getAddressUtxos address
    pure (latestBlocks, utxos)

  pPrint latestBlocks
  pPrint utxos
  pure $ toApiTxOut era address <$> utxos

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
  let era = toCardanoEra w
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
hexToByteString txt = BS.pack . map (sneakyHead . readHex . T.unpack) $ T.chunksOf 2 txt
  where
    sneakyHead ((p, _) : _) = p
    sneakyHead [] = error $ "Cannot decode hex from: " <> T.unpack txt

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
   in fromList
        [
          ( AssetId (fromString $ Text.unpack pId) assetName
          , fromInteger q
          )
        ]

signTransaction ::
  HasCallStack =>
  MonadFail m =>
  ShelleyBasedEra era ->
  -- | signing key in Base16
  Text ->
  -- | transaction body in JSON envelope
  LBS.ByteString ->
  m (Tx era)
signTransaction era signingKeyB16 txBodyJson = do
  let key = WitnessPaymentKey $ either (error . ("Cannot deserialize signing key from CBOR: " <>)) id $ do
        signingKeyBytes <- Base16.decode $ T.encodeUtf8 signingKeyB16
        first show $ deserialiseFromCBOR (AsSigningKey AsPaymentKey) signingKeyBytes

  InAnyShelleyBasedEra sbe tx <- pure . unCddlTx $ readCddlTx txBodyJson
  Just Refl <- pure $ testEquality era sbe
  let txBody = getTxBody tx
      keyWitness = makeShelleyKeyWitness era txBody key
  pure $ makeSignedTransaction [keyWitness] txBody

readCddlTx :: LBS.ByteString -> CddlTx
readCddlTx envelopeJson = do
  let envelope =
        either (error . ("Cannot decode envelope JSON: " <>)) id $
          A.eitherDecode' envelopeJson
  either (error . ("Cannot decode envelope object: " <>) . show) id $
    deserialiseFromTextEnvelopeCddlAnyOf teTypes envelope
  where
    teTypes =
      [ FromCDDLTx "Witnessed Tx ShelleyEra" CddlTx
      , FromCDDLTx "Witnessed Tx AllegraEra" CddlTx
      , FromCDDLTx "Witnessed Tx MaryEra" CddlTx
      , FromCDDLTx "Witnessed Tx AlonzoEra" CddlTx
      , FromCDDLTx "Witnessed Tx BabbageEra" CddlTx
      , FromCDDLTx "Witnessed Tx ConwayEra" CddlTx
      , FromCDDLTx "Unwitnessed Tx ByronEra" CddlTx
      , FromCDDLTx "Unwitnessed Tx ShelleyEra" CddlTx
      , FromCDDLTx "Unwitnessed Tx AllegraEra" CddlTx
      , FromCDDLTx "Unwitnessed Tx MaryEra" CddlTx
      , FromCDDLTx "Unwitnessed Tx AlonzoEra" CddlTx
      , FromCDDLTx "Unwitnessed Tx BabbageEra" CddlTx
      , FromCDDLTx "Unwitnessed Tx ConwayEra" CddlTx
      ]
