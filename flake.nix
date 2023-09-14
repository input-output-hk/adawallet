{
  description = "adawallet";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    cardano-node.url = "github:input-output-hk/cardano-node/8.1.2";
    cardano-addresses.url = "github:input-output-hk/cardano-addresses";
  };
  outputs = { self, nixpkgs, cardano-node, cardano-addresses, ... }@inputs: let
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
