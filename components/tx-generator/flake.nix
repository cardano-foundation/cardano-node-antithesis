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
      # Pinned to upstream main with the full
      # reconnect-resilience stack merged in:
      #   * PR #105 — N2C reconnect supervisor + BlockedIndefinitelyOnSTM catch
      #   * PR #110 — post-reconnect indexer freshness gate
      #   * PR #114 — pre-submit chain-tip UTxO probe
      #   * PR #115 — refill duplicate-submit recovery (await change-output on ConnectionLost / "already-included")
      #   * PR #116 — recovery-await timeout aligned with dcAwaitTimeoutSeconds
      #   * PR #117 — refill recovery-await timeout → IndexNotReady (transient retry, not Always-fire SubmitRejected)
      #   * PR #118 — same recovery shape applied to the transact arm
      # Per the *Pins main only* rule.
      url =
        "github:lambdasistemi/cardano-node-clients/711eb22ac03e67b753f7ce70e635cddcf6f3cdce";
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
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, cardano-node-clients, composer-sdk, ... }:
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
              composer-sdk-src = composer-sdk;
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
