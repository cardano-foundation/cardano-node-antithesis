{ pkgs, utxo-indexer, version, ... }:
let
  # Bake composer scripts under the canonical Antithesis composer
  # path. Antithesis discovers parallel_driver_*, eventually_*,
  # finally_* by walking /opt/antithesis/test/v1/<template>/.
  composer = pkgs.runCommand "asteria-stub-composer" { } ''
    mkdir -p $out/opt/antithesis/test/v1
    cp -r ${../composer}/. $out/opt/antithesis/test/v1
    chmod 0755 $out/opt/antithesis/test/v1/*/*.sh
  '';

  # /usr/bin/env shim — many shebangs assume it.
  usrBinEnv = pkgs.runCommand "usr-bin-env" { } ''
    mkdir -p $out/usr/bin
    ln -s ${pkgs.coreutils}/bin/env $out/usr/bin/env
  '';

  # Default args for utxo-indexer on this testnet (slot=1s,
  # magic=42, byron-epoch-slots=86400 per testnet.yaml +
  # oura-daemon.toml). Compose can override individual flags via
  # `command:`. --db-path enables RocksDB persistence so the
  # daemon resumes from last applied block on restart (which is
  # how we mitigate upstream issue #97 until a fix lands).
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
  name = "ghcr.io/cardano-foundation/cardano-node-antithesis/asteria-stub";
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
      composer
      usrBinEnv
    ];
  };
  config = {
    Entrypoint = [ "${utxo-indexer}/bin/utxo-indexer" ];
    Cmd = defaultIndexerArgs;
    WorkingDir = "/";
  };
}
