# AccountBalanceRequest

An AccountBalanceRequest is utilized to make a balance request on the /account/balance endpoint. If the block_identifier is populated, a historical balance query should be performed.
## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**network_identifier** | [**NetworkIdentifier**](NetworkIdentifier.md) |  | 
**account_identifier** | [**AccountIdentifier**](AccountIdentifier.md) |  | 
**block_identifier** | [**PartialBlockIdentifier**](PartialBlockIdentifier.md) |  | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


