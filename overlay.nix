let
  sources = import ./nix/sources.nix {};
  nodePkgs = import sources.cardano-node {};
  walletPkgs = import sources.cardano-wallet {};

in self: super: {
  cardano-hw-cli = self.callPackage ./cardano-hw-cli {};
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

  adawallet = self.python3Packages.buildPythonApplication {
    pname = "adawallet";
    version = "1.0.0";
    src = ./adawallet;
    propagatedBuildInputs = with self; [
      cardano-rosetta-py
      python3Packages.docopt
      python3Packages.apsw
    ];
    doCheck = false;
  };

  cardano-rosetta-py = let
    src = self.runCommand "cardano-rosetta-py-src" { buildInputs = [ self.openapi-generator-cli ]; } ''
      mkdir $out
      cd $out
      openapi-generator-cli generate -i ${sources.cardano-rosetta}/cardano-rosetta-server/src/server/openApi.json -g python --additional-properties=packageName=cardano_rosetta
    '';
  in self.python3Packages.buildPythonPackage {
    inherit src;
    version = "0.0.0";
    #nativeBuildInputs = with self.python3Packages; [ six ];
    propagatedBuildInputs = with self.python3Packages; [ certifi urllib3 dateutil ];
    pname = "cardano-rosetta-py";
    doCheck = false;
  };
}
