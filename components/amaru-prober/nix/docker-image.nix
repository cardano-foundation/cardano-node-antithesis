{ pkgs, version, ... }:
let
  composer = pkgs.runCommand "amaru-prober-composer" { } ''
    mkdir -p $out/opt/antithesis/test/v1
    cp -r ${../composer}/. $out/opt/antithesis/test/v1
    chmod 0755 $out/opt/antithesis/test/v1/*/*.sh
  '';

  amaru-prober = pkgs.writeShellApplication {
    name = "amaru-prober";
    runtimeInputs = [ pkgs.bash pkgs.coreutils pkgs.jq ];
    text = builtins.readFile ../amaru-prober.sh;
  };

  usrBinEnv = pkgs.runCommand "usr-bin-env" { } ''
    mkdir -p $out/usr/bin
    ln -s ${pkgs.coreutils}/bin/env $out/usr/bin/env
  '';
in
pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/cardano-node-antithesis/amaru-prober";
  tag = version;
  copyToRoot = pkgs.buildEnv {
    name = "amaru-prober-image-root";
    paths = [
      pkgs.bash
      pkgs.coreutils
      pkgs.jq
      amaru-prober
      composer
      usrBinEnv
    ];
  };
  config = {
    Entrypoint = [ "/bin/amaru-prober" ];
    WorkingDir = "/";
  };
}
