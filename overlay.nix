let
  sources = import ./nix/sources.nix {};
  nodePkgs = import sources.cardano-node {};
  walletPkgs = import sources.cardano-wallet {};

in self: super: {
  cardano-hw-cli = self.callPackage ./cardano-hw-cli {};
  cardano-rosetta-python = self.python3Packages.buildPythonPackage {
    pname = "cardano-rosetta-python";
    version = "0.0.0";
    src = ./cardano-rosetta-python;
    doCheck = false;
    propagatedBuildInputs = with self.python3Packages; [ six dateutil urllib3 ];
  };

  cardano-completions = self.runCommand "cardano-completions" {} ''
    BASH_COMPLETIONS=$out/share/bash-completion/completions
    mkdir -p $BASH_COMPLETIONS
    ${self.cardano-cli}/bin/cardano-cli --bash-completion-script cardano-cli > $BASH_COMPLETIONS/cardano-cli
    ${self.cardano-node}/bin/cardano-node --bash-completion-script cardano-node > $BASH_COMPLETIONS/cardano-node
    ${self.cardano-wallet}/bin/cardano-wallet --bash-completion-script cardano-wallet > $BASH_COMPLETIONS/cardano-wallet
    ${self.cardano-address}/bin/cardano-address --bash-completion-script cardano-address > $BASH_COMPLETIONS/cardano-address
    ${self.bech32}/bin/bech32 --bash-completion-script bech32 > $BASH_COMPLETIONS/bech32
  '';

  inherit (nodePkgs) cardano-node cardano-cli;
  inherit (walletPkgs) cardano-address bech32;
  # cardano-wallet attribute includes cardano-node which causes a collision
  inherit (walletPkgs.haskellPackages.cardano-wallet.components.exes) cardano-wallet;
  trezor = self.python3Packages.trezor.overrideAttrs (oldAttrs: {
    src = self.python3Packages.fetchPypi {
      pname = "trezor";
      version = "0.12.1";
      sha256 = "sha256-KTz8PF0T+mKkLSP4XaoMmOPrLjxEqwylTrMUzWmqKfA=";
    };
  });
  query-utxo-accounts = self.python3Packages.buildPythonApplication {
    pname = "query-utxo-accounts";
    version = "0.0.0";
    src = ./query-utxo-accounts;
    propagatedBuildInputs = [ self.cardano-rosetta-python self.python3Packages.docopt ];
    doCheck = false;
  };
  adawallet = self.python3Packages.buildPythonApplication {
    pname = "adawallet";
    version = "0.0.0";
    src = ./adawallet;
    propagatedBuildInputs = [ self.query-utxo-accounts self.python3Packages.docopt ];
    doCheck = false;
  };
}
