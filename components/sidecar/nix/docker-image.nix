{ pkgs, version, adversary-exe, cardano-cli, ... }:
let
  # Copy over the antithesis test scripts
  antithesis-assets = pkgs.runCommand "antithesis-assets" { } ''
    mkdir -p $out/opt/antithesis/test/v1/
    cp -r ${../composer}/.  $out/opt/antithesis/test/v1
    chmod 0755 $out/opt/antithesis/test/v1/*/*
  '';

  # Create a wrapper script for the flaky chain sync driver (for testing purposes)
  flaky-chain-sync = pkgs.writeShellScriptBin "flaky-chain-sync" ''
    exec ${antithesis-assets}/opt/antithesis/test/v1/chain-sync-client/parallel_driver_flaky_chain_sync.sh "$@"'';
  eventually-converged = pkgs.writeShellScriptBin "eventually-converged" ''
    exec ${antithesis-assets}/opt/antithesis/test/v1/convergence/eventually_converged.sh "$@"'';

  # Sleep script to keep the container running
  sleep = pkgs.writeShellScriptBin "sleep-forever" ''
    #!/bin/sh
    tail -f /dev/null
  '';
  # Make sure /usr/bin/env is available in the image
  usrBinEnv = pkgs.runCommand "usr-bin-env" { } ''
    mkdir -p $out/usr/bin
    ln -s ${pkgs.coreutils}/bin/env $out/usr/bin/env
  '';
in pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/cardano-node-antithesis/sidecar";
  tag = version;
  config = {
    EntryPoint = [ "/bin/sleep-forever" ];
    Tmpfs = { "/tmp" = "rw,noexec,nosuid,nodev,size=64m"; };
  };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [
      pkgs.coreutils
      pkgs.bash
      pkgs.jq
      usrBinEnv
      sleep
      antithesis-assets
      flaky-chain-sync
      eventually-converged
      adversary-exe
      cardano-cli
    ];
  };
}
