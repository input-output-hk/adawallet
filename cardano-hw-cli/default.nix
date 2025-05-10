# Starting with cardano-hw-cli release v1.11.0, this project's build tool
# packaging changed from `nexe` to `pkg`.  This complicates packaging options
# for nixos since `pkg` built bins are not ordinary binaries, but complex virtual
# runtimes with embedded bytecode, entrypoints and its own virtual FS.
#
# Practically speaking, this prevents patchelf application to release binaries
# because it breaks the binary's bootstrap image entrypoint.  Alternatively,
# simply trying to wrap the `pkg` built binary into an FHS nix packaged build
# is also problematic due to dynamic `pkg` runtime FS assumptions which aren't
# guessable at build time.
#
# Building from source is the remaining viable option, although the build needs
# to be modified from the standard approach, otherwise the default build
# product is again a `pkg` binary which won't run on nixos and can't be
# patchelf'd.
#
# Additionally the latest release at the time of this writing still requires
# quite an old node version which comes with it's own challenges such as:
#
#   * The nixpkgs associated with the required node version has yarn2nix
#   infrastructure doesn't support sha512 yarn hashes even though the project
#   itself has adopted them in their yarn.lock file.
#
#   * Newer yarn2nix nixpkgs build infrastructure isn't easily portable to old
#   nixpkgs
#
#   * Attempting to run an older node version on a newer nixpkgs for better
#   yarn2nix support yields failed node compilation without node source
#   patching
#
# For now, the path of least resistance to a working packaged source build of
# the latest release appears to be converting the project's yarn.lock to sha1
# only usage (see ./convert-sha512-to-sha1.py) and using an older nixpkgs pin
# which contains the appropriate node version.
#
# There are still some remaining caveats with this, such as the yarn2nix
# packaging not supporting scoped or early escaped package urls properly.  This
# seems to be an issue in both old and new nixpkgs.  To address this, the
# ../nix/yarn2nix-mod/ files contain patches which support scoped and early
# escaped package urls.
#
# When this project bumps their node requirement to a newer version, building
# will get easier!

{pkgs}: let
  inherit (yarn2nixMod) mkYarnPackage mkYarnModules mkYarnNix importOfflineCache;

  yarn2nixMod = import ../nix/yarn2nix-mod {inherit pkgs;};

  name = "cardano-hw-cli";
  hwCliVersion = "1.18.2";

  # Currently node 18.7.0 is required
  nodejs = pkgs.nodejs-18_x;

  src = pkgs.fetchFromGitHub {
    owner = "vacuumlabs";
    repo = "cardano-hw-cli";
    rev = "refs/tags/v${hwCliVersion}";
    sha256 = "sha256:0chqx4cx6vkd3ixdayaz8ssw2lj7sym5n4h0xwcwy82riq2n4mgj";
  };

  yarnLock = ./yarn.lock.patched-${hwCliVersion};

  yarnNix = mkYarnNix {inherit yarnLock;};

  offlineCache' = importOfflineCache yarnNix;

  yarnDeps = mkYarnModules {
    inherit yarnLock;

    pname = "${name}-modules";
    version = "v${hwCliVersion}";

    packageJSON = "${src}/package.json";
  };
in
  with pkgs;
    mkYarnPackage rec {
      inherit nodejs src yarnLock yarnNix;

      pname = name;
      version = "v${hwCliVersion}";

      packageJSON = "${src}/package.json";

      offlineCache = "./offline";

      yarnPreBuild = ''
        # Yarn expects the offline cache relative to $HOME
        mkdir -p "$HOME/${offlineCache}"

        # Yarn later tries to mkdir this dir and will fail if symlinked from top level
        find "${offlineCache'}" -maxdepth 1 -type l -exec ln -s {} "$HOME/${offlineCache}" \;
      '';

      nativeBuildInputs = [
        autoconf
        automake
        gcc
        libtool
        makeWrapper
        nodejs
        pkg-config
        python3
      ];

      buildInputs = [
        libusb1
        systemd
      ];

      buildPhase = ''
        runHook preBuild
        cd deps/cardano-hw-cli
        yarn build-js
        cd -
        runHook postBuild
      '';

      installPhase = ''
        runHook preInstall

        mkdir -p "$out/bin"
        mkdir -p "$out/share/cardano-hw-cli"
        mkdir -p "$out/share/bash-completion/completions"

        # Required for index.js startup
        cp -r deps/cardano-hw-cli/dist "$out/share/cardano-hw-cli/"

        # Helpful!
        cp -r deps/cardano-hw-cli/docs "$out/share/cardano-hw-cli/"

        # Required for the `version` command
        cp deps/cardano-hw-cli/package.json "$out/share/cardano-hw-cli/"

        # Required for bash completion
        cp deps/cardano-hw-cli/scripts/autocomplete.sh "$out/share/bash-completion/completions/cardano-hw-cli"


        ln -sv "${yarnDeps}/node_modules" "$out/share/cardano-hw-cli/node_modules"

        makeWrapper ${nodejs}/bin/node "$out/bin/cardano-hw-cli" \
          --set NODE_PATH "$out/share/cardano-hw-cli/node_modules" \
          --set LD_LIBRARY_PATH "${systemd}/lib" \
          --add-flags "$out/share/cardano-hw-cli/dist/index.js"

        runHook postInstall
      '';

      distPhase = "true";
    }
