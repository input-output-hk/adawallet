# ConstructionPreprocessRequest

ConstructionPreprocessRequest is passed to the `/construction/preprocess` endpoint so that a Rosetta implementation can determine which metadata it needs to request for construction. Metadata provided in this object should NEVER be a product of live data (i.e. the caller must follow some network-specific data fetching strategy outside of the Construction API to populate required Metadata). If live data is required for construction, it MUST be fetched in the call to `/construction/metadata`. The caller can provide a max fee they are willing to pay for a transaction. This is an array in the case fees must be paid in multiple currencies. The caller can also provide a suggested fee multiplier to indicate that the suggested fee should be scaled. This may be used to set higher fees for urgent transactions or to pay lower fees when there is less urgency. It is assumed that providing a very low multiplier (like 0.0001) will never lead to a transaction being created with a fee less than the minimum network fee (if applicable). In the case that the caller provides both a max fee and a suggested fee multiplier, the max fee will set an upper bound on the suggested fee (regardless of the multiplier provided).
## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**network_identifier** | [**NetworkIdentifier**](NetworkIdentifier.md) |  | 
**operations** | [**list[Operation]**](Operation.md) |  | 
**metadata** | [**ConstructionPreprocessRequestMetadata**](ConstructionPreprocessRequestMetadata.md) |  | [optional] 
**max_fee** | [**list[Amount]**](Amount.md) |  | [optional] 
**suggested_fee_multiplier** | **float** |  | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


