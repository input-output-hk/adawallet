{
  description = "adawallet";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    cardano-parts.url = "github:input-output-hk/cardano-parts";
    cardano-node.url = "github:intersectmbo/cardano-node/8.10.0-pre";
    nixpkgs.follows = "cardano-parts/nixpkgs";
  };
  outputs = { self, flake-utils, nixpkgs, ... }@inputs: let
  overlay = import ./overlay.nix { inherit inputs self; };
  in flake-utils.lib.eachDefaultSystem (system: let
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ overlay ];
    };

  in {
    legacyPackages = pkgs;
    devShell = pkgs.devShell;
  }) // {
    inherit overlay;
  };
}
