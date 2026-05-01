{ pkgs, project, version, utxo-indexer, ... }:
let
  # Bake the antithesis composer scripts under the canonical
  # /opt/antithesis/test/v1/<template>/ path. The Antithesis composer
  # discovers parallel_driver_*, serial_driver_*, eventually_*,
  # finally_* by walking this tree.
  antithesis-drivers = pkgs.runCommand "antithesis-drivers" { } ''
    mkdir -p $out/opt/antithesis/test/v1/
    cp -r ${../composer}/. $out/opt/antithesis/test/v1
    chmod 0755 $out/opt/antithesis/test/v1/*/*.sh
  '';

  usrBinEnv = pkgs.runCommand "usr-bin-env" { } ''
    mkdir -p $out/usr/bin
    ln -s ${pkgs.coreutils}/bin/env $out/usr/bin/env
  '';

  # Default args for utxo-indexer on this testnet (slot=1s,
  # magic=42, byron-epoch-slots=86400 per testnet.yaml +
  # oura-daemon.toml). Compose can override via `command:`.
  # --db-path enables RocksDB persistence so the daemon resumes
  # from last applied block on restart.
  defaultIndexerArgs = [
    "--relay-socket"          "/state/node.socket"
    "--listen"                "/tmp/idx.sock"
    "--network-magic"         "42"
    "--byron-epoch-slots"     "86400"
    "--ready-threshold-slots" "5"
    "--security-param-k"      "432"
    "--db-path"               "/idx-db"
  ];
in
pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/cardano-node-antithesis/asteria-game";
  tag = version;
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [
      pkgs.bash
      pkgs.coreutils
      pkgs.gnugrep
      pkgs.jq
      pkgs.netcat-openbsd
      pkgs.socat
      utxo-indexer
      antithesis-drivers
      usrBinEnv
      project.packages.asteria-game
      project.packages.asteria-bootstrap
    ];
  };
  config = {
    Entrypoint = [ "${utxo-indexer}/bin/utxo-indexer" ];
    Cmd = defaultIndexerArgs;
    WorkingDir = "/";
  };
}
