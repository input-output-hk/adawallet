# CoinChange

CoinChange is used to represent a change in state of a some coin identified by a coin_identifier. This object is part of the Operation model and must be populated for UTXO-based blockchains. Coincidentally, this abstraction of UTXOs allows for supporting both account-based transfers and UTXO-based transfers on the same blockchain (when a transfer is account-based, don't populate this model).
## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**coin_identifier** | [**CoinIdentifier**](CoinIdentifier.md) |  | 
**coin_action** | [**CoinAction**](CoinAction.md) |  | 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


