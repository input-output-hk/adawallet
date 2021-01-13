# SigningPayload

SigningPayload is signed by the client with the keypair associated with an AccountIdentifier using the specified SignatureType. SignatureType can be optionally populated if there is a restriction on the signature scheme that can be used to sign the payload.
## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**address** | **str** | [DEPRECATED by &#x60;account_identifier&#x60; in &#x60;v1.4.4&#x60;] The network-specific address of the account that should sign the payload. | [optional] 
**account_identifier** | [**AccountIdentifier**](AccountIdentifier.md) |  | [optional] 
**hex_bytes** | **str** |  | 
**signature_type** | [**SignatureType**](SignatureType.md) |  | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


