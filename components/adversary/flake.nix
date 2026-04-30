{
  description =
    "adversary — Antithesis docker image for the cardano-node-clients adversary daemon";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    cardano-node-clients = {
      # Pinned to the merge commit of
      # https://github.com/lambdasistemi/cardano-node-clients/pull/106
      # on main per the *Pins main only* rule.
      url =
        "github:lambdasistemi/cardano-node-clients/60ef7733f13c29882d936c87565b64edde189975";
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, cardano-node-clients, ... }:
    let
      version = self.dirtyShortRev or self.shortRev or "dev";
      parts = flake-parts.lib.mkFlake { inherit inputs; } {
        systems = [ "x86_64-linux" "aarch64-darwin" ];
        perSystem = { system, ... }:
          let
            pkgs = import nixpkgs { inherit system; };
            adversary-bin =
              cardano-node-clients.packages.${system}.cardano-adversary;
            docker-image = pkgs.callPackage ./nix/docker-image.nix {
              inherit adversary-bin version;
            };
          in {
            packages = {
              inherit docker-image adversary-bin;
              default = docker-image;
            };
          };
      };
    in {
      inherit (parts) packages;
      inherit version;
    };
}
