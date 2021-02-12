let
  sources = import ./nix/sources.nix {};
  pkgs = import sources.nixpkgs { overlays = [ (import ./overlay.nix) ]; };
  nativeBuildInputs = with pkgs; [
    cardano-cli
    cardano-hw-cli
    cardano-address
    cardano-completions
    python3Packages.ipython
    python3Packages.apsw
    cardano-rosetta-py
    adawallet
    srm
  ];
in pkgs.mkShell {
  inherit nativeBuildInputs;
  XDG_DATA_DIRS = with pkgs.lib; concatStringsSep ":" (
    [(builtins.getEnv "XDG_DATA_DIRS")] ++
    (filter
      (share: builtins.pathExists (share + "/bash-completion"))
      (map (p: p + "/share") nativeBuildInputs))
  );
  shellHook = ''
    echo "Ada Wallet Shell Tools" \
    | ${pkgs.figlet}/bin/figlet -f banner -c \
    | ${pkgs.lolcat}/bin/lolcat
  '';
} // { inherit nativeBuildInputs pkgs; }
