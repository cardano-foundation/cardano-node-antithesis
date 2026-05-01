{
  description =
    "tx-generator — Antithesis docker image for the cardano-node-clients tx-generator daemon";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    cardano-node-clients = {
      # Pinned to the main merge commit of
      # https://github.com/lambdasistemi/cardano-node-clients/pull/105.
      # This keeps the Antithesis image on the N2C reconnect
      # supervisor and the BlockedIndefinitelyOnSTM -> ConnectionLost
      # hardening observed necessary in one-hour fault runs.
      url =
        "github:lambdasistemi/cardano-node-clients/898a2c470ced6a82fa5a32b18cbaf195e1cce927";
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
            tx-generator-bin =
              cardano-node-clients.packages.${system}.cardano-tx-generator;
            docker-image = pkgs.callPackage ./nix/docker-image.nix {
              inherit tx-generator-bin version;
            };
          in {
            packages = {
              inherit docker-image tx-generator-bin;
              default = docker-image;
            };
          };
      };
    in {
      inherit (parts) packages;
      inherit version;
    };
}
