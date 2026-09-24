{
  description = "sidecar for adversary testing on Cardano nodes";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    cardano-node-runtime.url = "github:IntersectMBO/cardano-node?ref=10.1.4";
    adversary.url =
      "github:cardano-foundation/cardano-node-antithesis?dir=components/adversary";
    image-meta = {
      url = "path:../image-meta";
      flake = false;
    };
  };

  outputs =
    inputs@{ self, flake-parts, nixpkgs, cardano-node-runtime, adversary
    , image-meta, ... }:
    let
      imageMeta = import (image-meta + "/meta.nix") {
        inherit self;
        sourceUrl =
          "https://github.com/cardano-foundation/cardano-node-antithesis";
      };
      version = imageMeta.version;
      parts = flake-parts.lib.mkFlake { inherit inputs; } {
        systems = [ "x86_64-linux" "aarch64-darwin" ];
        perSystem = { system, pkgs, ... }:
          let
            cardano-cli =
              cardano-node-runtime.project.${system}.pkgs.cardano-cli;
            adversary-exe = adversary.packages.${system}.adversary;
            sidecar-image = pkgs.callPackage ./nix/docker-image.nix {
              inherit version adversary-exe cardano-cli imageMeta;
            };
          in {
            packages = {
              default = sidecar-image;
              docker-image = sidecar-image;
            };
            devShells.default = pkgs.mkShell {
              buildInputs = [
                adversary-exe
                cardano-cli
                pkgs.just
                pkgs.nixfmt-classic
                pkgs.shellcheck
              ];
            };
          };
      };
    in {
      inherit (parts) packages devShells;
      inherit version;
    };
}
