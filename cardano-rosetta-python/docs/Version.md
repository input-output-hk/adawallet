# Version

The Version object is utilized to inform the client of the versions of different components of the Rosetta implementation.
## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**rosetta_version** | **str** | The rosetta_version is the version of the Rosetta interface the implementation adheres to. This can be useful for clients looking to reliably parse responses. | 
**node_version** | **str** | The node_version is the canonical version of the node runtime. This can help clients manage deployments. | 
**middleware_version** | **str** | When a middleware server is used to adhere to the Rosetta interface, it should return its version here. This can help clients manage deployments. | [optional] 
**metadata** | [**object**](.md) | Any other information that may be useful about versioning of dependent services should be returned here. | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


