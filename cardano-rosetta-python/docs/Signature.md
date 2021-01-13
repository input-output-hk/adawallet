# Signature

Signature contains the payload that was signed, the public keys of the keypairs used to produce the signature, the signature (encoded in hex), and the SignatureType. PublicKey is often times not known during construction of the signing payloads but may be needed to combine signatures properly.
## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**signing_payload** | [**SigningPayload**](SigningPayload.md) |  | 
**public_key** | [**PublicKey**](PublicKey.md) |  | 
**signature_type** | [**SignatureType**](SignatureType.md) |  | 
**hex_bytes** | **str** |  | 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


