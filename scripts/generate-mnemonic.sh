#!/bin/bash

scriptdir=$(dirname "$0")
sawdir="$scriptdir/saw"

# Check if the directory exists
if [ -d "$sawdir" ]; then
    echo "Warning: Single address wallet directory '$sawdir' already exists."
    exit 1  # Exit with a non-zero status to indicate an error
fi

mkdir -p "$sawdir"
mnemonicfile=""$sawdir/recovery-phrase.prv""
cardano-address recovery-phrase generate > "$mnemonicfile"
echo "Wallet mnemonic generated: $mnemonicfile"