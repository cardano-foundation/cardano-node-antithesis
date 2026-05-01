{ pkgs, adversary-bin, version, ... }:
let
  # Bake the antithesis composer scripts under the canonical
  # /opt/antithesis/test/v1/chain-sync-client/ path. The Antithesis
  # composer discovers parallel_driver_*, eventually_*, finally_* by
  # walking this tree. Same idiom as components/tx-generator.
  #
  # Files whose basename starts with "helper_" are skipped by the
  # composer dispatcher and only sourced by sibling scripts; they
  # don't need exec bits, but giving them the same chmod is harmless
  # and matches tx-generator's recipe.
  antithesis-drivers = pkgs.runCommand "antithesis-drivers" { } ''
    mkdir -p $out/opt/antithesis/test/v1/
    cp -r ${../composer}/. $out/opt/antithesis/test/v1
    chmod 0755 $out/opt/antithesis/test/v1/*/parallel_driver_*.sh \
               $out/opt/antithesis/test/v1/*/eventually_*.sh \
               $out/opt/antithesis/test/v1/*/finally_*.sh
  '';

  # Wrapper bins so docker-compose's `command:` and `exec` can invoke
  # the drivers by short name without knowing the path.
  parallel-driver-flaky-chain-sync =
    pkgs.writeShellScriptBin "parallel_driver_flaky_chain_sync" ''
      exec ${antithesis-drivers}/opt/antithesis/test/v1/chain-sync-client/parallel_driver_flaky_chain_sync.sh "$@"
    '';
  eventually-adversary-active =
    pkgs.writeShellScriptBin "eventually_adversary_active" ''
      exec ${antithesis-drivers}/opt/antithesis/test/v1/chain-sync-client/eventually_adversary_active.sh "$@"
    '';
  finally-adversary-summary =
    pkgs.writeShellScriptBin "finally_adversary_summary" ''
      exec ${antithesis-drivers}/opt/antithesis/test/v1/chain-sync-client/finally_adversary_summary.sh "$@"
    '';

  usrBinEnv = pkgs.runCommand "usr-bin-env" { } ''
    mkdir -p $out/usr/bin
    ln -s ${pkgs.coreutils}/bin/env $out/usr/bin/env
  '';
in
pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/cardano-node-antithesis/adversary";
  tag = version;
  config = {
    EntryPoint = [ "/bin/cardano-adversary" ];
    Labels = {
      "org.opencontainers.image.documentation" =
        "https://github.com/lambdasistemi/cardano-node-clients/blob/main/specs/036-cardano-adversary/contracts/control-wire.md";
    };
  };
  copyToRoot = pkgs.buildEnv {
    name = "adversary-image-root";
    paths = [
      pkgs.coreutils
      pkgs.bash
      pkgs.jq
      pkgs.gnugrep
      pkgs.netcat-openbsd
      usrBinEnv
      antithesis-drivers
      parallel-driver-flaky-chain-sync
      eventually-adversary-active
      finally-adversary-summary
      adversary-bin
    ];
  };
}
