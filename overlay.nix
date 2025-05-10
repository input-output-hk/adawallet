{ inputs, self }:
let
  nodePkgs = inputs.nodePkgs.legacyPackages.x86_64-linux;
in final: prev: {
  inherit (inputs.cardano-parts.packages.x86_64-linux) cardano-node cardano-cli bech32 cardano-address;

  adawallet = final.python3Packages.buildPythonApplication {
    pname = "adawallet";
    version = "1.0.0";
    src = ./adawallet;

    propagatedBuildInputs = with final; [
      blockfrost
      python3Packages.apsw
      python3Packages.docopt
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

  # nodePkgs provides node 18.7.0 explicitly required by cardano-hw-cli
  cardano-hw-cli = nodePkgs.callPackage ./cardano-hw-cli {};

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
      adawallet
      blockfrost
      cardano-address
      cardano-cli
      cardano-node
      cardano-hw-cli
      python3Packages.apsw
      python3Packages.ipython
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
