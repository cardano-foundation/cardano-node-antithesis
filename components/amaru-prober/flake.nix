{
  description = "amaru-prober — Antithesis composer carrier for Amaru relay startup proofs";

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    let
      version = self.dirtyShortRev or self.shortRev or "dev";
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      perSystem = { pkgs, system, ... }: {
        packages.docker-image = pkgs.callPackage ./nix/docker-image.nix {
          inherit version;
        };
        packages.default = self.packages.${system}.docker-image;
      };
      flake = { inherit version; };
    };
}
