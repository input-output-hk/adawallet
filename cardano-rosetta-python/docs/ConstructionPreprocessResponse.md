# ConstructionPreprocessResponse

ConstructionPreprocessResponse contains `options` that will be sent unmodified to `/construction/metadata`. If it is not necessary to make a request to `/construction/metadata`, `options` should be omitted.  Some blockchains require the PublicKey of particular AccountIdentifiers to construct a valid transaction. To fetch these PublicKeys, populate `required_public_keys` with the AccountIdentifiers associated with the desired PublicKeys. If it is not necessary to retrieve any PublicKeys for construction, `required_public_keys` should be omitted.
## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**options** | [**ConstructionPreprocessResponseOptions**](ConstructionPreprocessResponseOptions.md) |  | [optional] 
**required_public_keys** | [**list[AccountIdentifier]**](AccountIdentifier.md) |  | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


