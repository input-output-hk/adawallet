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
# quite an old nodejs version which comes with its own challenges such as:
#
#   * The nixpkgs associated with the required nodejs version has yarn2nix
#   infrastructure that doesn't support sha512 yarn hashes even though the
#   project itself has adopted them in their yarn.lock file.
#
#   * Attempting to run an older nodejs version on a newer nixpkgs for better
#   yarn2nix support yields failed nodejs compilation without nodejs source
#   patching
#
#   * Newer yarn2nix nixpkgs build infrastructure may not be easily portable to
#   old nixpkgs; this should probably be the next exploratory improvement step
#   when time permits as it might not be that much work with perhaps only some
#   light patching required.
#
# For now, the path of least resistance to a working packaged source build of
# the latest release appears to be converting the project's yarn.lock to sha1
# only usage (see ./convert-sha512-to-sha1.py) and using an older nixpkgs pin
# which contains the appropriate nodejs version.
#
# There are still some remaining caveats with this, such as:
#
#   * The yarn2nix packaging not supporting scoped or early escaped package
#   urls properly.  This seems to be an issue in both old and new nixpkgs.  To
#   address this, the ../nix/yarn2nix-mod/ files contain patches which support
#   scoped and early escaped package urls.
#
#   * Cardano-hw-cli requires not only the primary `cardano-hw-cli` build
#   product, but also native node-hid bindings, `HID_hidraw.node` and
#   `HID.node`, which require node-gyp postinstall build compilation which in
#   turn requires nodejs headers.  Unfortunately, this old nodejs version doesn't
#   include the required headers in the nixpkg so we need to roll our own
#   nodejs dev package to support node-gyp native add-on compilation.
#
#   * Iterating on source compiled nodejs package builds requires > 1 hr per
#   build.  Ccache can reduce this to a much more manageable minute or two
#   after the first build, enabling fast(er) prototyping.  See ccache usage
#   notes below.
#
# When this project bumps their nodejs requirement to a newer version, nix
# packaging will get easier!
#
{pkgs}: let
  inherit (pkgs.lib) licenses optionalString;
  inherit (yarn2nixMod) mkYarnPackage mkYarnModules mkYarnNix importOfflineCache;

  # When building locally with uncached variants, fresh nodejs source builds can
  # take > 1 hr. Using ccache, build iteration can be brought down to a minute
  # or two. If useCcache is set true, a nixos system will also need some
  # minimal config, such as:
  #
  #   {pkgs, config, ...}: {
  #     nix.settings.extra-sandbox-paths = [config.programs.ccache.cacheDir];
  #
  #     programs.ccache = {
  #       enable = true;
  #       cacheDir = "/var/cache/ccache";
  #     };
  #
  #     environment.systemPackages = [pkgs.ccache];
  #   }
  #
  # For other setups, see docs such as: https://nixos.wiki/wiki/CCache
  #
  # Once prototyping is done, this should be set false, otherwise, hydra and
  # other envs not configured for ccache usage will fail to build.
  useCcache = false;

  yarn2nixMod = import ../nix/yarn2nix-mod {inherit pkgs;};

  name = "cardano-hw-cli";
  hwCliVersion = "1.18.2";

  # Currently node 18.7.0 is required
  nodejsWithDev = pkgs.nodejs-18_x.overrideAttrs (old: {
    outputs = ["out" "dev"];

    nativeBuildInputs = with pkgs;
      (old.nativeBuildInputs or [])
      ++ [
        binutils
        ccache
        icu
        openssl
        pkg-config
        python3
        zlib
      ];

    configurePhase = ''
      ${optionalString useCcache ''
        mkdir -p ./wrapped-bin
        for compiler in gcc g++ cc c++; do
          ln -sf ${pkgs.ccache}/libexec/ccache/$compiler ./wrapped-bin/$compiler
        done
        export PATH=$PWD/wrapped-bin:$PATH

        export SRC="$PWD"
        export CCACHE_DIR="/var/cache/ccache"
        export CCACHE_LOGFILE=$TMPDIR/ccache.log
        export CC="ccache gcc"
        export CXX="ccache g++"
        export CC_host="ccache gcc"
        export CXX_host="ccache g++"

        echo "Using ccache for builds"
        which ccache || echo "WARNING: ccache not found"

        echo "PATH: $PATH"
        echo "CC: $CC"
        echo "CXX: $CXX"
        echo "CCACHE_DIR: $CCACHE_DIR"
        echo "Resolved c++: $(${pkgs.which}/bin/which c++)"
        echo "Resolved gcc: $(${pkgs.which}/bin/which gcc)"
        ccache -p
      ''}

      ./configure --prefix=$out
    '';

    buildPhase = ''
      make -j$NIX_BUILD_CORES
    '';

    installPhase = ''
      # Prevent global install attempts to /lib, /usr, etc
      export PREFIX=$out
      export DESTDIR=$out

      mkdir -p $out/bin $out/include $out/lib
      make install

      cp out/Release/node $out/bin/node
      chmod +x $out/bin/node

      mkdir -p $out/lib/node_modules
      cp -r deps/npm $out/lib/node_modules/
      ln -sf $out/lib/node_modules/npm/bin/npm-cli.js $out/bin/npm
      ln -sf $out/lib/node_modules/npm/bin/npx-cli.js $out/bin/npx

      substituteInPlace $out/lib/node_modules/npm/bin/npm-cli.js \
        --replace "#!/usr/bin/env node" "#!${pkgs.nodejs-18_x}/bin/node"

      substituteInPlace $out/lib/node_modules/npm/bin/npx-cli.js \
        --replace "#!/usr/bin/env node" "#!${pkgs.nodejs-18_x}/bin/node"

      # Include dev outputs required for node-gyp native add-on compilation
      mkdir -p $dev/include/node
      cp src/*.h $dev/include/node/
      cp -r deps/v8/include/* $dev/include/node/
      cp config.gypi common.gypi $dev/include/node/
      [ -d deps/openssl ] && cp -r deps/openssl/openssl/include/openssl $dev/include/node/
    '';
  });

  nodejs = nodejsWithDev;

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
        libusb1
        makeWrapper
        nodejs
        pkg-config
        python3
        systemd
      ];

      buildPhase = ''
        runHook preBuild

        export HOME="$TMPDIR"
        export YARN_CACHE_FOLDER="$TMPDIR/yarn-cache"
        mkdir -p "$YARN_CACHE_FOLDER"

        cd deps/cardano-hw-cli
        yarn --offline build-js
        cd -

        runHook postBuild
      '';

      installPhase = ''
        runHook preInstall

        # Package the primary `cardano-hw-cli` product
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

      postInstall = ''
        export HOME=$(mktemp -d)
        export npm_config_nodedir=${nodejs.dev}

        tmpdir="$TMPDIR/cardano-hw-cli-rebuild"
        mkdir -p "$tmpdir"
        cp -r $out/share/cardano-hw-cli $tmpdir/app
        cd $tmpdir/app

        mkdir build-node-hid
        cp -r node_modules/node-hid/* build-node-hid/
        chmod -R u+w build-node-hid
        (cd build-node-hid && \
          ${nodejs}/bin/node ${nodejs}/lib/node_modules/npm/node_modules/node-gyp/bin/node-gyp.js rebuild)

        echo "Using Node: $(node -v)"
        echo "Using node-gyp: ${nodejs}/lib/node_modules/npm/node_modules/node-gyp/bin/node-gyp.js"
        echo "Using node headers at: $npm_config_nodedir"

        # Make fresh node_modules
        cp -rTL node_modules new_node_modules
        chmod -R u+w new_node_modules
        rm -rf new_node_modules/node-hid
        cp -r build-node-hid new_node_modules/node-hid

        # Replace store-backed node_modules
        rm -rf $out/share/cardano-hw-cli/node_modules
        cp -r new_node_modules $out/share/cardano-hw-cli/node_modules

        # Package the native add-ons
        mkdir -p $out/bin/Release
        cp new_node_modules/node-hid/build/Release/HID.node $out/bin/Release
        cp new_node_modules/node-hid/build/Release/HID_hidraw.node $out/bin/Release
      '';

      # V8 ABI compatible shared objects often don't play nice with patchelf and shrinks
      preFixup = ''
        echo "Adding a filtering wrapper for patchelf"
        mkdir -p $TMPDIR/wrapped-bin

        cat > $TMPDIR/wrapped-bin/patchelf <<EOF
      #!/bin/sh
      case "\$1" in
        --print-rpath)
          case "\$2" in
            *.node|*.o)
              echo "skipping patchelf on <...>/\$(basename \$2) as an excluded filetype..." >&2
              exit 0
              ;;
          esac
          ;;
        --shrink-rpath)
          case "\$2" in
            *.node|*.o)
              echo "skipping shrinking on <...>/\$(basename \$2) as an excluded filetype" >&2
              exit 0
              ;;
          esac
          ;;
      esac
      exec ${pkgs.patchelf}/bin/patchelf "\$@"
      EOF

      chmod +x $TMPDIR/wrapped-bin/*
      export PATH="$TMPDIR/wrapped-bin:$PATH"
      '';

      distPhase = ''
        echo "Skipping distPhase for a top-level build"
      '';

      meta = {
        description = "Cardano CLI tool for hardware wallets";
        homepage = "https://github.com/vacuumlabs/cardano-hw-cli";
        license = licenses.isc;
        mainProgram = "cardano-hw-cli";
      };
    }
