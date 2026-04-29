{ pkgs, tx-generator-bin, version, ... }:
let
  # Bake the antithesis composer scripts under the
  # canonical /opt/antithesis/test/v1/tx-generator/ path.
  # The Antithesis composer discovers parallel_driver_*,
  # eventually_*, finally_* by walking this tree. Same
  # idiom as components/asteria-player.
  antithesis-drivers = pkgs.runCommand "antithesis-drivers" { } ''
    mkdir -p $out/opt/antithesis/test/v1/
    cp -r ${../composer}/. $out/opt/antithesis/test/v1
    chmod 0755 $out/opt/antithesis/test/v1/*/parallel_driver_*.sh \
               $out/opt/antithesis/test/v1/*/eventually_*.sh \
               $out/opt/antithesis/test/v1/*/finally_*.sh
  '';

  # Wrapper bins so docker-compose's `command:` and
  # `exec` can invoke the drivers by short name without
  # knowing the path.
  parallel-driver-transact = pkgs.writeShellScriptBin "parallel_driver_transact" ''
    exec ${antithesis-drivers}/opt/antithesis/test/v1/tx-generator/parallel_driver_transact.sh "$@"
  '';
  parallel-driver-refill = pkgs.writeShellScriptBin "parallel_driver_refill" ''
    exec ${antithesis-drivers}/opt/antithesis/test/v1/tx-generator/parallel_driver_refill.sh "$@"
  '';
  eventually-population-grew = pkgs.writeShellScriptBin "eventually_population_grew" ''
    exec ${antithesis-drivers}/opt/antithesis/test/v1/tx-generator/eventually_population_grew.sh "$@"
  '';
  finally-pressure-summary = pkgs.writeShellScriptBin "finally_pressure_summary" ''
    exec ${antithesis-drivers}/opt/antithesis/test/v1/tx-generator/finally_pressure_summary.sh "$@"
  '';

  usrBinEnv = pkgs.runCommand "usr-bin-env" { } ''
    mkdir -p $out/usr/bin
    ln -s ${pkgs.coreutils}/bin/env $out/usr/bin/env
  '';

  # The container brings up the long-running daemon; the
  # composer drivers shell into it via `docker compose exec`
  # (or the antithesis composer model). Sleep-forever lets
  # the daemon also be started with no args for inspection
  # in compose.
  sleepForever = pkgs.writeShellScriptBin "sleep-forever" ''
    #!/bin/sh
    tail -f /dev/null
  '';
in pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/cardano-node-antithesis/tx-generator";
  tag = version;
  config = {
    EntryPoint = [ "/bin/cardano-tx-generator" ];
  };
  copyToRoot = pkgs.buildEnv {
    name = "tx-generator-image-root";
    paths = [
      pkgs.coreutils
      pkgs.bash
      pkgs.jq
      pkgs.gnugrep
      pkgs.netcat-openbsd
      usrBinEnv
      sleepForever
      antithesis-drivers
      parallel-driver-transact
      parallel-driver-refill
      eventually-population-grew
      finally-pressure-summary
      tx-generator-bin
    ];
  };
}
