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

    # Pinned to PR #98 head (035-indexer-n2c-reconnect): supervisor +
    # exponential-backoff reconnect on N2C peer close. Resolves the
    # upstream issue #97
    # (https://github.com/lambdasistemi/cardano-node-clients/issues/97).
    # Bump again once #98 merges to main and we want the merge commit.
    cardano-node-clients = {
      url = "github:lambdasistemi/cardano-node-clients/5707836b623918043a7f2fbdcc2ed499902ac082";
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
