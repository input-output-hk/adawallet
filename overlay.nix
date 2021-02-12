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
    postInstall = ''
      mkdir -p $out/share/bash-completion/completions
      ${self.docopt_completion}/bin/docopt-completion $out/bin/adawallet --manual-bash
      # docopt-completion generates trailing = which breaks completion
      cat adawallet.sh|${self.gnused}/bin/sed 's/= / /g' > $out/share/bash-completion/completions/adawallet
      mkdir -p $out/share/zsh/site-functions
      ${self.docopt_completion}/bin/docopt-completion $out/bin/adawallet --manual-zsh
      cp _adawallet $out/share/zsh/site-functions/_adawallet
    '';
  };

  docopt_completion = self.python3Packages.buildPythonApplication rec {
    pname = "infi.docopt_completion";
    version = "0.2.9";
    propagatedBuildInputs = [
      self.python3Packages.docopt
    ];
    src = self.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "sha256-x+ZXVNQ9NrIM1jZGDDx3ZgeOUv7NqqZLoZmiaB5KbOI=";
    };
    doCheck = false;
  };

  nulltype = self.python3Packages.buildPythonApplication rec {
    pname = "nulltype";
    version = "2.3.1";
    src = self.fetchurl {
      url = "https://files.pythonhosted.org/packages/2f/ce/92289851364b7f816a839c8064aac06c01f3a3ecf33ab04adf9d0a0ab66a/nulltype-2.3.1.zip";
      sha256 = "sha256-ZKo8sqtekE0bNxdbm5Ir6iaME/nOMuPTczExUKte8nI=";
    };
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
    propagatedBuildInputs = with self.python3Packages; [ certifi urllib3 dateutil self.nulltype ];
    pname = "cardano-rosetta-py";
    doCheck = false;
  };
}
