# ConstructionMetadataRequest

A ConstructionMetadataRequest is utilized to get information required to construct a transaction. The Options object used to specify which metadata to return is left purposely unstructured to allow flexibility for implementers. Optionally, the request can also include an array of PublicKeys associated with the AccountIdentifiers returned in ConstructionPreprocessResponse.
## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**network_identifier** | [**NetworkIdentifier**](NetworkIdentifier.md) |  | 
**options** | [**ConstructionMetadataRequestOptions**](ConstructionMetadataRequestOptions.md) |  | 
**public_keys** | [**list[PublicKey]**](PublicKey.md) |  | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


