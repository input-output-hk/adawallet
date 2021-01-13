# ConstructionPayloadsRequest

ConstructionPayloadsRequest is the request to `/construction/payloads`. It contains the network, a slice of operations, and arbitrary metadata that was returned by the call to `/construction/metadata`. Optionally, the request can also include an array of PublicKeys associated with the AccountIdentifiers returned in ConstructionPreprocessResponse.
## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**network_identifier** | [**NetworkIdentifier**](NetworkIdentifier.md) |  | 
**operations** | [**list[Operation]**](Operation.md) |  | 
**metadata** | [**ConstructionPayloadsRequestMetadata**](ConstructionPayloadsRequestMetadata.md) |  | 
**public_keys** | [**list[PublicKey]**](PublicKey.md) |  | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


