#!/usr/bin/env bash

scriptdir=$(dirname "$0")
sawdir="$scriptdir/saw"

if [ ! -d "$sawdir" ]; then
    echo "Error: Wallet directory '$sawdir' does not exist. Generate a wallet first with generate-mnemonic.sh"
    exit 1
fi

# Read mnemonic from file
recovery_phrasefp="$sawdir/recovery-phrase.prv"

# Construct root extended private key
# https://github.com/uniVocity/cardano-tutorials/blob/master/cardano-addresses.md#generating-private-keys
#
rootextendedprivatekeyfp=$sawdir/root.xprv
cardano-address key from-recovery-phrase Shelley < "$recovery_phrasefp" > "$rootextendedprivatekeyfp"
echo "Root extended private key: $rootextendedprivatekeyfp"

# Construct extended private key for account ix=0H, role=0 and address ix=0
# https://github.com/uniVocity/cardano-tutorials/blob/master/cardano-addresses.md#understanding-the-hd-wallet-address-format-bip-44
#
extendedsigningkey="$sawdir/key.xsk"
cardano-address key child 1852H/1815H/0H/0/0 < "$rootextendedprivatekeyfp" > "$extendedsigningkey"
echo "Extended signing key: $extendedsigningkey"

clisigningkeyfile="$sawdir/cli_extended_key.skey"
cliverificationkeyfile="$sawdir/cli_extended_key.vkey"

# Create extended signing key using cardano-cli
cardano-cli key convert-cardano-address-key \
  --shelley-payment-key --signing-key-file "$extendedsigningkey" \
  --out-file "$clisigningkeyfile"

echo "Cli signing key: $clisigningkeyfile"

cardano-cli key verification-key \
  --signing-key-file "$clisigningkeyfile" \
  --verification-key-file "$cliverificationkeyfile"

echo "Cli verification key: $sawdir/$cliverificationkeyfile"
