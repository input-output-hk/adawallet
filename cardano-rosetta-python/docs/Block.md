# Block

Blocks contain an array of Transactions that occurred at a particular BlockIdentifier. A hard requirement for blocks returned by Rosetta implementations is that they MUST be _inalterable_: once a client has requested and received a block identified by a specific BlockIndentifier, all future calls for that same BlockIdentifier must return the same block contents.
## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**block_identifier** | [**BlockIdentifier**](BlockIdentifier.md) |  | 
**parent_block_identifier** | [**BlockIdentifier**](BlockIdentifier.md) |  | 
**timestamp** | **int** | The timestamp of the block in milliseconds since the Unix Epoch. The timestamp is stored in milliseconds because some blockchains produce blocks more often than once a second. | 
**transactions** | [**list[Transaction]**](Transaction.md) |  | 
**metadata** | [**BlockMetadata**](BlockMetadata.md) |  | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


