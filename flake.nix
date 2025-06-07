{
  description = "adawallet";

  inputs = {
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

    systems.url = "github:nix-systems/x86_64-linux";

    cardano-parts.url = "github:input-output-hk/cardano-parts/v2025-05-22";

    nixpkgs.follows = "cardano-parts/nixpkgs";

    # Nixpkgs w/ nodejs 18.7.0, explicitly required by cardano-hw-cli
    nodePkgs.url = "github:NixOS/nixpkgs/7753a94a35438b73a1aa1d8aca233753367ff4d6";
  };

  outputs = {
    self,
    flake-utils,
    nixpkgs,
    cardano-parts,
    nodePkgs,
    ...
  } @ inputs: let
    overlay = import ./overlay.nix {inherit inputs self;};
  in
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [overlay];
      };
    in rec {
      checks = {};

      devShells.default = pkgs.devShell;

      legacyPackages = pkgs;

      packages = {
        inherit
          (legacyPackages)
          adawallet
          blockfrost
          cardano-address
          cardano-cli
          cardano-hw-cli
          ;
      };

      hydraJobs = let
        inherit (pkgs.lib) collect isDerivation;
        jobs = {inherit packages checks devShells;};
      in
        jobs
        // {
          required = pkgs.releaseTools.aggregate {
            name = "required";
            constituents = collect isDerivation jobs;
          };
        };
    })
    // {
      inherit overlay;
    };
}
