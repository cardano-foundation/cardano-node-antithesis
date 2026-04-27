{
  description = "asteria-stub — long-lived utxo-indexer + composer harness for Antithesis";

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";

    # Pinned to PR #94 head (034-cardano-tx-generator) — that branch
    # carries the utxo-indexer + the tx-generator's consumer integration
    # pattern we mirror. Bump only after upstream issue #97
    # (https://github.com/lambdasistemi/cardano-node-clients/issues/97 —
    # bearer-closed auto-reconnect) is resolved.
    cardano-node-clients = {
      url = "github:lambdasistemi/cardano-node-clients/d80ab59fdd3634546a694163f2fb15ca1bffcfdc";
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, cardano-node-clients, ... }:
    let
      version = self.dirtyShortRev or self.shortRev or "dev";
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      perSystem = { pkgs, system, ... }: {
        packages.docker-image = pkgs.callPackage ./nix/docker-image.nix {
          inherit version;
          utxo-indexer = cardano-node-clients.packages.${system}.utxo-indexer;
        };
        packages.default = self.packages.${system}.docker-image;
      };
      flake = { inherit version; };
    };
}
