# Operation

Operations contain all balance-changing information within a transaction. They are always one-sided (only affect 1 AccountIdentifier) and can succeed or fail independently from a Transaction.
## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**operation_identifier** | [**OperationIdentifier**](OperationIdentifier.md) |  | 
**related_operations** | [**list[OperationIdentifier]**](OperationIdentifier.md) | Restrict referenced related_operations to identifier indexes &lt; the current operation_identifier.index. This ensures there exists a clear DAG-structure of relations. Since operations are one-sided, one could imagine relating operations in a single transfer or linking operations in a call tree. | [optional] 
**type** | **str** | The network-specific type of the operation. Ensure that any type that can be returned here is also specified in the NetworkOptionsResponse. This can be very useful to downstream consumers that parse all block data. | 
**status** | **str** | The network-specific status of the operation. Status is not defined on the transaction object because blockchains with smart contracts may have transactions that partially apply. Blockchains with atomic transactions (all operations succeed or all operations fail) will have the same status for each operation. | [optional] 
**account** | [**AccountIdentifier**](AccountIdentifier.md) |  | [optional] 
**amount** | [**Amount**](Amount.md) |  | [optional] 
**coin_change** | [**CoinChange**](CoinChange.md) |  | [optional] 
**metadata** | [**OperationMetadata**](OperationMetadata.md) |  | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


