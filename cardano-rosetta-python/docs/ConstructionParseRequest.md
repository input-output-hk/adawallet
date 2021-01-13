# ConstructionParseRequest

ConstructionParseRequest is the input to the `/construction/parse` endpoint. It allows the caller to parse either an unsigned or signed transaction.
## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**network_identifier** | [**NetworkIdentifier**](NetworkIdentifier.md) |  | 
**signed** | **bool** | Signed is a boolean indicating whether the transaction is signed. | 
**transaction** | **str** | This must be either the unsigned transaction blob returned by &#x60;/construction/payloads&#x60; or the signed transaction blob returned by &#x60;/construction/combine&#x60;. | 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


