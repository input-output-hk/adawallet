# ConstructionPayloadsResponse

ConstructionTransactionResponse is returned by `/construction/payloads`. It contains an unsigned transaction blob (that is usually needed to construct the a network transaction from a collection of signatures) and an array of payloads that must be signed by the caller.
## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**unsigned_transaction** | **str** |  | 
**payloads** | [**list[SigningPayload]**](SigningPayload.md) |  | 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


