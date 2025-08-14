{ inputs, self }: final: prev: {
  inherit (inputs.cardano-parts.packages.x86_64-linux) cardano-node cardano-cli cardano-signer bech32 cardano-address;

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

    meta = {
      description = "A single address wallet that supports mnemonics and hardware wallets";
      homepage = "https://github.com/input-output-hk/adawallet";
      license = final.lib.licenses.asl20;
      mainProgram = "adawallet";
    };
  };

  cardano-hw-cli = final.callPackage ./cardano-hw-cli {};

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
    pname = "blockfrost_python";
    version = "0.6.0";

    propagatedBuildInputs = [
      final.python3Packages.requests
      final.python3Packages.setuptools
    ];

    src = final.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "sha256-dkt5Vheq39cSsqIU+mvSbMoz8ACDQOAiUSbRi+BAsRI=";
    };

    doCheck = false;
  };

  devShellAdawallet = prev.mkShell {
    nativeBuildInputs = with final; [
      adawallet
      blockfrost
      cardano-address
      cardano-cli
      cardano-node
      cardano-hw-cli
      cardano-signer
      python3Packages.apsw
      python3Packages.ipython
      sqlite-interactive
    ];

    shellHook = ''
      echo "Ada Wallet Shell Tools" \
      | ${final.figlet}/bin/figlet -f banner -c \
      | ${final.clolcat}/bin/clolcat
    '';
    };

  devShellYarn = prev.mkShell {
    nativeBuildInputs = with final; [
      nodejs
      yarn
      yarn2nix
    ];

    shellHook = ''
      echo "Ada Wallet Yarn Tools" \
      | ${final.figlet}/bin/figlet -f banner -c \
      | ${final.clolcat}/bin/clolcat
    '';
    };
}
