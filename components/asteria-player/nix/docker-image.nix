{ pkgs, project, version, ... }:
let
  # Bake the antithesis composer scripts under the canonical
  # /opt/antithesis/test/v1/asteria/ path. The Antithesis composer
  # discovers parallel_driver_*, eventually_*, finally_* by walking
  # this tree.
  antithesis-drivers = pkgs.runCommand "antithesis-drivers" { } ''
    mkdir -p $out/opt/antithesis/test/v1/
    cp -r ${../composer}/. $out/opt/antithesis/test/v1
    chmod 0755 $out/opt/antithesis/test/v1/*/parallel_driver_*.sh \
               $out/opt/antithesis/test/v1/*/eventually_*.sh
  '';

  # Wrapper bins so docker-compose's `command` and `exec` can
  # invoke the drivers by short name without knowing the path.
  parallel-driver-bootstrap = pkgs.writeShellScriptBin "parallel_driver_asteria_bootstrap" ''
    exec ${antithesis-drivers}/opt/antithesis/test/v1/asteria/parallel_driver_asteria_bootstrap.sh "$@"
  '';
  parallel-driver-player = pkgs.writeShellScriptBin "parallel_driver_asteria_player" ''
    exec ${antithesis-drivers}/opt/antithesis/test/v1/asteria/parallel_driver_asteria_player.sh "$@"
  '';
  eventually-asteria-alive = pkgs.writeShellScriptBin "eventually_asteria_alive" ''
    exec ${antithesis-drivers}/opt/antithesis/test/v1/asteria/eventually_asteria_alive.sh "$@"
  '';

  usrBinEnv = pkgs.runCommand "usr-bin-env" { } ''
    mkdir -p $out/usr/bin
    ln -s ${pkgs.coreutils}/bin/env $out/usr/bin/env
  '';

  # Keep the container alive between bootstrap (one-shot) and
  # player (long-running) so docker-compose can also start the
  # image with no command for inspection.
  sleepForever = pkgs.writeShellScriptBin "sleep-forever" ''
    #!/bin/sh
    tail -f /dev/null
  '';
in pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/cardano-node-antithesis/asteria-player";
  tag = version;
  config = { EntryPoint = [ "/bin/sleep-forever" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [
      pkgs.coreutils
      pkgs.bash
      pkgs.jq
      pkgs.gnugrep
      usrBinEnv
      sleepForever
      antithesis-drivers
      parallel-driver-bootstrap
      parallel-driver-player
      eventually-asteria-alive
      project.packages.asteria-player.package.components.exes.asteria-player
      project.packages.asteria-player.package.components.exes.asteria-bootstrap
    ];
  };
}
