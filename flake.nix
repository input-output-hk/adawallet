{
  description = "adawallet";
  inputs = {
    utils.url = "github:kreisys/flake-utils";
    nixpkgs.follows = "cardano-node/nixpkgs";
    cardano-node.url = "github:input-output-hk/cardano-node/1.29.0";
    cardano-addresses.url = "github:input-output-hk/cardano-addresses/nix-flake";
    cardano-rosetta = {
      url = "github:input-output-hk/cardano-rosetta/1.4.0";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, utils, cardano-node, cardano-addresses, cardano-rosetta }@inputs:
    utils.lib.simpleFlake {
      inherit nixpkgs;
      systems = [ "x86_64-linux" "x86_64-darwin" ];
      overlay = import ./overlay.nix { inherit inputs self; };
      packages = { adawallet }: {
        inherit adawallet;
        defaultPackage = adawallet;
      };

      shell = { devShell }: devShell;
    };
}
