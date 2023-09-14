{ inputs, self }:
final: prev:
let
  inherit (final) lib;
in {
  cardano-hw-cli = final.callPackage ./cardano-hw-cli {};
  cardano-completions = final.runCommand "cardano-completions" {} ''
    BASH_COMPLETIONS=$out/share/bash-completion/completions
    mkdir -p $BASH_COMPLETIONS
    ${final.cardano-cli}/bin/cardano-cli --bash-completion-script cardano-cli > $BASH_COMPLETIONS/cardano-cli
    ${final.cardano-node}/bin/cardano-node --bash-completion-script cardano-node > $BASH_COMPLETIONS/cardano-node
    ${final.cardano-addresses-cli}/bin/cardano-address --bash-completion-script cardano-address > $BASH_COMPLETIONS/cardano-address
    ${final.bech32}/bin/bech32 --bash-completion-script bech32 > $BASH_COMPLETIONS/bech32
  '';

  inherit (inputs.cardano-node.legacyPackages.x86_64-linux) cardano-node cardano-cli bech32;

  cardano-addresses-cli = inputs.cardano-addresses.packages.x86_64-linux."cardano-addresses-cli:exe:cardano-address";

  adawallet = final.python3Packages.buildPythonApplication {
    pname = "adawallet";
    version = "1.0.0";
    src = ./adawallet;
    propagatedBuildInputs = with final; [
      python3Packages.docopt
      python3Packages.apsw
      blockfrost
    ];
    doCheck = false;
    postInstall = ''
      mkdir -p $out/share/bash-completion/completions
      ${final.docopt_completion}/bin/docopt-completion $out/bin/adawallet --manual-bash
      # docopt-completion generates trailing = which breaks completion
      cat adawallet.sh|${final.gnused}/bin/sed 's/= / /g' > $out/share/bash-completion/completions/adawallet
      mkdir -p $out/share/zsh/site-functions
      ${final.docopt_completion}/bin/docopt-completion $out/bin/adawallet --manual-zsh
      cp _adawallet $out/share/zsh/site-functions/_adawallet
    '';
  };

  docopt_completion = final.python3Packages.buildPythonApplication rec {
    pname = "infi.docopt_completion";
    version = "0.2.9";
    propagatedBuildInputs = [
      final.python3Packages.docopt
    ];
    src = final.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "sha256-x+ZXVNQ9NrIM1jZGDDx3ZgeOUv7NqqZLoZmiaB5KbOI=";
    };
    doCheck = false;
  };
  blockfrost = final.python3Packages.buildPythonPackage rec {
    pname = "blockfrost-python";
    version = "0.5.3";
    propagatedBuildInputs = [
      final.python3Packages.requests
      final.python3Packages.setuptools
    ];
    src = final.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "sha256-MVS5mGfncUyQBkyeGjfjt6+XwQe2RUndDUJKqjIJAX4=";
    };
    doCheck = false;
  };

  devShell = prev.mkShell rec {
    nativeBuildInputs = with final; [
      cardano-cli
      cardano-hw-cli
      cardano-addresses-cli
      cardano-completions
      python3Packages.ipython
      python3Packages.apsw
      blockfrost
      adawallet
    ];
    XDG_DATA_DIRS = with final.lib; concatStringsSep ":" (
      [(builtins.getEnv "XDG_DATA_DIRS")] ++
      (filter
        (share: builtins.pathExists (share + "/bash-completion"))
        (map (p: p + "/share") nativeBuildInputs))
    );
    shellHook = ''
      echo "Ada Wallet Shell Tools" \
      | ${final.figlet}/bin/figlet -f banner -c \
      | ${final.lolcat}/bin/lolcat
    '';
    };
}
