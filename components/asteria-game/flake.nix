{
  description =
    "asteria-game — utxo-indexer + bootstrap + player for the cardano-node-antithesis cluster";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  # Pinned to match cardano-node-clients's flake inputs so the
  # haskell.nix project plan resolves identically. Bumping
  # cardano-node-clients's tag in cabal.project should be
  # accompanied by a refresh of these revisions.
  inputs = {
    haskellNix = {
      url =
        "github:input-output-hk/haskell.nix/8b447d7f57d62fab9249f79bb916bc891e29b9d0";
      inputs.hackage.follows = "hackageNix";
    };
    hackageNix = {
      url =
        "github:input-output-hk/hackage.nix/8a8899d3e55b6ea4b08be45a91228d1cc4c20d5c";
      flake = false;
    };
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    iohkNix = {
      url =
        "github:input-output-hk/iohk-nix/f444d972c301ddd9f23eac4325ffcc8b5766eee9";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    CHaP = {
      url =
        "github:intersectmbo/cardano-haskell-packages/f150513ff4741d1b816b26694425f19cddb67109";
      flake = false;
    };

    # The daemon and game binaries share the same node-client revision.
    cardano-node-clients = {
      url =
        "github:lambdasistemi/cardano-node-clients/e17ffdc74c756680006d43963e337d9a766aa144";
    };
    cardano-tx-tools = {
      url =
        "github:lambdasistemi/cardano-tx-tools/2e0d7cb813a9509c6677debdfe5fdad2a76351ff";
      flake = false;
    };
    # Shared composer-SDK helper (signal hardening + sdk.jsonl emit).
    # `flake = false` keeps this a raw source path; the docker-image
    # derivation copies `helper_sdk_common.sh` next to the per-component
    # helper so the runtime `source $(dirname "$0")/helper_sdk_common.sh`
    # resolves at composer execution time.
    composer-sdk = {
      url = "path:../composer-sdk";
      flake = false;
    };
    image-meta = {
      url = "path:../image-meta";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, haskellNix, hackageNix, CHaP
    , iohkNix, cardano-node-clients, cardano-tx-tools, composer-sdk, image-meta
    , ... }:
    let
      imageMeta = import (image-meta + "/meta.nix") {
        inherit self;
        sourceUrl =
          "https://github.com/cardano-foundation/cardano-node-antithesis";
      };
      version = imageMeta.version;
      parts = flake-parts.lib.mkFlake { inherit inputs; } {
        systems = [ "x86_64-linux" ];
        perSystem = { system, ... }:
          let
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
              indexState = "2026-09-29T11:48:37Z";
              inherit CHaP pkgs cardano-node-clients cardano-tx-tools;
            };
            docker-image = pkgs.callPackage ./nix/docker-image.nix {
              inherit project version imageMeta;
              utxo-indexer =
                cardano-node-clients.packages.${system}.utxo-indexer;
              composer-sdk-src = composer-sdk;
            };
          in rec {
            packages = {
              inherit (project.packages)
                asteria-game asteria-bootstrap asteria-invariant;
              inherit docker-image;
              default = packages.docker-image;
            };
            inherit (project) devShells;
          };
      };
    in {
      inherit (parts) packages devShells;
      inherit version;
    };
}
