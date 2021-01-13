# AccountBalanceResponse

An AccountBalanceResponse is returned on the /account/balance endpoint. If an account has a balance for each AccountIdentifier describing it (ex: an ERC-20 token balance on a few smart contracts), an account balance request must be made with each AccountIdentifier.
## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**block_identifier** | [**BlockIdentifier**](BlockIdentifier.md) |  | 
**balances** | [**list[Amount]**](Amount.md) | A single account may have a balance in multiple currencies. | 
**coins** | [**list[Coin]**](Coin.md) | If a blockchain is UTXO-based, all unspent Coins owned by an account_identifier should be returned alongside the balance. It is highly recommended to populate this field so that users of the Rosetta API implementation don&#39;t need to maintain their own indexer to track their UTXOs. | [optional] 
**metadata** | [**object**](.md) | Account-based blockchains that utilize a nonce or sequence number should include that number in the metadata. This number could be unique to the identifier or global across the account address. | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


