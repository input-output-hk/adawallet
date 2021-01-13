# Allow

Allow specifies supported Operation status, Operation types, and all possible error statuses. This Allow object is used by clients to validate the correctness of a Rosetta Server implementation. It is expected that these clients will error if they receive some response that contains any of the above information that is not specified here.
## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**operation_statuses** | [**list[OperationStatus]**](OperationStatus.md) | All Operation.Status this implementation supports. Any status that is returned during parsing that is not listed here will cause client validation to error. | 
**operation_types** | **list[str]** | All Operation.Type this implementation supports. Any type that is returned during parsing that is not listed here will cause client validation to error. | 
**errors** | [**list[Error]**](Error.md) | All Errors that this implementation could return. Any error that is returned during parsing that is not listed here will cause client validation to error. | 
**historical_balance_lookup** | **bool** | Any Rosetta implementation that supports querying the balance of an account at any height in the past should set this to true. | 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


