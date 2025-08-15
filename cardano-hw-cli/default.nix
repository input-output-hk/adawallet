# The upstream source recommends building with nodejs 18 which is EOL.  Doing
# so is possible but introduces lots of packaging complications which are
# difficult to work around.
#
# Using the default nodejs with a recent nixpkgs also works, is much more
# straight-forward and doesn't have any CVEs the old yarn2nix modules carry
# with the included yarn.lock file.
{pkgs}: let
  inherit (pkgs.lib) licenses;

  name = "cardano-hw-cli";
  hwCliVersion = "1.18.2";

  src = pkgs.fetchFromGitHub {
    owner = "vacuumlabs";
    repo = "cardano-hw-cli";
    rev = "refs/tags/v${hwCliVersion}";
    sha256 = "sha256:0chqx4cx6vkd3ixdayaz8ssw2lj7sym5n4h0xwcwy82riq2n4mgj";
  };

in
  with pkgs;
    mkYarnPackage rec {
      inherit src;

      pname = name;
      version = "v${hwCliVersion}";

      nativeBuildInputs = [
        autoconf
        automake
        gcc
        libtool
        libusb1
        makeWrapper
        node-gyp
        nodejs
        pkg-config
        python3
      ];

      postBuild = ''
        # Use ignore-engines as the upstream is expecting an EOL, unmaintained
        # nodejs version which we don't want to use.
        cd deps/${pname}
        yarn --offline --ignore-engines --frozen-lockfile build-js
        cd -

        # Rebuild the node-hid module, otherwise the default generated
        # HID*.node files don't connect to the ledger properly.
        for d in $(find . -type d -path "*/node_modules/node-hid"); do
          (cd "$d" && node-gyp rebuild)
        done
      '';

      postInstall = ''
        # Enable bash auto-completion
        mkdir -p "$out/share/bash-completion/completions"
        cp \
          "$out/libexec/${pname}/deps/${pname}/scripts/autocomplete.sh" \
          "$out/share/bash-completion/completions/cardano-hw-cli"

        # Make an executable wrapper at the standard nix closure bin path
        makeWrapper ${nodejs}/bin/node "$out/bin/${pname}" \
          --set NODE_PATH "$out/libexec/${pname}/node_modules" \
          --set LD_LIBRARY_PATH "${systemd}/lib" \
          --add-flags "$out/libexec/${pname}/deps/${pname}/dist/index.js"
      '';

      meta = {
        description = "Cardano CLI tool for hardware wallets";
        homepage = "https://github.com/vacuumlabs/cardano-hw-cli";
        license = licenses.isc;
        mainProgram = "cardano-hw-cli";
      };
    }
