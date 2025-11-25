{ pkgs, project, version, ... }:
let
  antithesis-setup = pkgs.runCommand "antithesis-setup" { } ''
    mkdir -p $out/opt/antithesis/test/v1/
    cp -r ${../composer}/chain-sync-client  $out/opt/antithesis/test/v1
    chmod 0755 $out/opt/antithesis/test/v1/*/*
  '';
  sleep = pkgs.writeShellScriptBin "sleep-forever" ''
    #!/bin/sh
    tail -f /dev/null
  '';
  usrBinEnv = pkgs.runCommand "usr-bin-env" {} ''
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
      project.packages.adversary.package.components.exes.adversary
      antithesis-setup
      sleep
    ];
  };
}
