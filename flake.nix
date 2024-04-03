{
  description = "adawallet";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    cardano-parts.url = "github:input-output-hk/cardano-parts";
    nixpkgs.follows = "cardano-parts/nixpkgs";
  };
  outputs = { self, flake-utils, nixpkgs, cardano-parts, ... }@inputs:
  flake-utils.lib.eachDefaultSystem (system: let
    overlay = import ./overlay.nix { inherit inputs self; };
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ overlay ];
    };

  in {
    inherit overlay;
    legacyPackages = pkgs;
    devShell = pkgs.devShell;
  });
}
