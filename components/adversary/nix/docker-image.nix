{ pkgs, project, version, ... }:
let
  # Copy over the antithesis test scripts
  antithesis-assets = pkgs.runCommand "antithesis-assets" { } ''
    mkdir -p $out/opt/antithesis/test/v1/
    cp -r ${../composer}/chain-sync-client  $out/opt/antithesis/test/v1
    chmod 0755 $out/opt/antithesis/test/v1/*/*
  '';
  # Create a wrapper script for the flaky chain sync driver (for testing purposes)
  antithesis-flaky-chain-sync-driver =
    pkgs.writeShellScriptBin "flaky-chain-sync-driver" ''
      exec ${antithesis-assets}/opt/antithesis/test/v1/chain-sync-client/parallel_driver_flaky_chain_sync.sh "$@"'';
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
  name = "ghcr.io/cardano-foundation/cardano-node-antithesis/adversary";
  tag = version;
  config = { EntryPoint = [ "/bin/sleep-forever" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [
      pkgs.coreutils
      pkgs.bash
      usrBinEnv
      sleep
      antithesis-assets
      antithesis-flaky-chain-sync-driver
      project.packages.adversary.package.components.exes.adversary
    ];
  };
}
