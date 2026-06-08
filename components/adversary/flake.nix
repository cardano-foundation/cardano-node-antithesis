{
  description = "adversary";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackageNix";
    };
    hackageNix = {
      url = "github:input-output-hk/hackage.nix/06fa3e96f4d7ced3496ec984c8016aad5282db67";
      flake = false;
    };
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    cardano-node-runtime = {
      url = "github:IntersectMBO/cardano-node?ref=11.0.1";
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      haskellNix,
      CHaP,
      iohkNix,
      cardano-node-runtime,
      ...
    }:
    let
      version = self.dirtyShortRev or self.shortRev;
      parts = flake-parts.lib.mkFlake { inherit inputs; } {
        systems = [
          "x86_64-linux"
          "aarch64-darwin"
        ];
        perSystem =
          { system, ... }:
          let
            node = cardano-node-runtime.project.${system};
            pkgs = import nixpkgs {
              overlays = [
                iohkNix.overlays.crypto
                haskellNix.overlay
                iohkNix.overlays.haskell-nix-crypto
                iohkNix.overlays.cardano-lib
              ];
              inherit system;
            };
            project = import ./nix/project.nix {
              indexState = "2026-03-26T20:21:33Z";
              inherit CHaP;
              inherit pkgs;
            };
            docker-image = pkgs.callPackage ./nix/docker-image.nix {
              inherit project version;
            };
          in
          rec {
            packages = {
              inherit (node.pkgs) cardano-node cardano-cli;
              inherit (project.packages)
                cardano-adversary
                adversary-tests
                ;
              inherit docker-image;
              default = packages.cardano-adversary;
            };
            inherit (project) devShells;
          };
      };
    in
    {
      inherit (parts) packages devShells;
      inherit version;
    };
}
