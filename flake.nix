{
  description = "adawallet";

  inputs = {
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

    systems.url = "github:nix-systems/x86_64-linux";

    cardano-parts.url = "github:input-output-hk/cardano-parts/v2025-08-05";

    nixpkgs.follows = "cardano-parts/nixpkgs";
  };

  outputs = {
    self,
    flake-utils,
    nixpkgs,
    cardano-parts,
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

      devShells = with pkgs; rec {
        default = adawallet;

        adawallet = devShellAdawallet;
        yarn = devShellYarn;
      };

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
