# SyncStatus

SyncStatus is used to provide additional context about an implementation's sync status. It is often used to indicate that an implementation is healthy when it cannot be queried  until some sync phase occurs. If an implementation is immediately queryable, this model is often not populated.
## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**current_index** | **int** | CurrentIndex is the index of the last synced block in the current stage. | 
**target_index** | **int** | TargetIndex is the index of the block that the implementation is attempting to sync to in the current stage. | [optional] 
**stage** | **str** | Stage is the phase of the sync process. | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


