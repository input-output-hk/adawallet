{
  description = "adawallet";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    cardano-node.url = "github:input-output-hk/cardano-node/nix-flake";
    cardano-addresses.url = "github:input-output-hk/cardano-addresses/nix-flake";
    cardano-rosetta = {
      url = "github:input-output-hk/cardano-rosetta/1.1.0";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, cardano-node, cardano-addresses, cardano-rosetta }@inputs: let
    overlay = import ./overlay.nix { inherit inputs self; };
    pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [ overlay ];
    };
  in {
    inherit overlay;
    legacyPackages.x86_64-linux = pkgs;
    devShell.x86_64-linux = pkgs.devShell;
  };
}
