# NetworkStatusResponse

NetworkStatusResponse contains basic information about the node's view of a blockchain network. It is assumed that any BlockIdentifier.Index less than or equal to CurrentBlockIdentifier.Index can be queried. If a Rosetta implementation prunes historical state, it should populate the optional `oldest_block_identifier` field with the oldest block available to query. If this is not populated, it is assumed that the `genesis_block_identifier` is the oldest queryable block. If a Rosetta implementation performs some pre-sync before it is possible to query blocks, sync_status should be populated so that clients can still monitor healthiness. Without this field, it may appear that the implementation is stuck syncing and needs to be terminated.
## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**current_block_identifier** | [**BlockIdentifier**](BlockIdentifier.md) |  | 
**current_block_timestamp** | **int** | The timestamp of the block in milliseconds since the Unix Epoch. The timestamp is stored in milliseconds because some blockchains produce blocks more often than once a second. | 
**genesis_block_identifier** | [**BlockIdentifier**](BlockIdentifier.md) |  | 
**oldest_block_identifier** | [**BlockIdentifier**](BlockIdentifier.md) |  | [optional] 
**sync_status** | [**SyncStatus**](SyncStatus.md) |  | [optional] 
**peers** | [**list[Peer]**](Peer.md) |  | 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


