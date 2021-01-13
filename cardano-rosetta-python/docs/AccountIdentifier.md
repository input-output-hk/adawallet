# AccountIdentifier

The account_identifier uniquely identifies an account within a network. All fields in the account_identifier are utilized to determine this uniqueness (including the metadata field, if populated).
## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**address** | **str** | The address may be a cryptographic public key (or some encoding of it) or a provided username. | 
**sub_account** | [**SubAccountIdentifier**](SubAccountIdentifier.md) |  | [optional] 
**metadata** | [**object**](.md) | Blockchains that utilize a username model (where the address is not a derivative of a cryptographic public key) should specify the public key(s) owned by the address in metadata. | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


