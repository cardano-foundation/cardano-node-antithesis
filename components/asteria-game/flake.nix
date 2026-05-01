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
        "github:input-output-hk/haskell.nix/ef52c36b9835c77a255befe2a20075ba71e3bfab";
      inputs.hackage.follows = "hackageNix";
    };
    hackageNix = {
      url =
        "github:input-output-hk/hackage.nix/55ba0ca4bcc9690f2ea45335cb2b9e95d8219a04";
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
        "github:intersectmbo/cardano-haskell-packages/887d73ce434831e3a67df48e070f4f979b3ac5a6";
      flake = false;
    };

    # Upstream utxo-indexer binary (PR #98 head — supervisor +
    # exponential-backoff reconnect on N2C peer close). Imported as
    # a flake input so dockerTools can pull the prebuilt binary +
    # closure without re-building from source.
    cardano-node-clients = {
      url =
        "github:lambdasistemi/cardano-node-clients/9db6672adb2f1ab8d01e493bac16f42396a03bb4";
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, haskellNix, hackageNix, CHaP
    , iohkNix, cardano-node-clients, ... }:
    let
      version = self.dirtyShortRev or self.shortRev or "dev";
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
              indexState = "2026-02-17T10:15:41Z";
              inherit CHaP pkgs;
            };
            docker-image = pkgs.callPackage ./nix/docker-image.nix {
              inherit project version;
              utxo-indexer = cardano-node-clients.packages.${system}.utxo-indexer;
            };
          in rec {
            packages = {
              inherit (project.packages) asteria-game asteria-bootstrap asteria-invariant;
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
