{
  description = "adawallet";
  inputs = {
    cardano-parts.url = "github:input-output-hk/cardano-parts";
    nixpkgs.follows = "cardano-parts/nixpkgs";
  };
  outputs = { self, nixpkgs, cardano-parts, ... }@inputs: let
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
