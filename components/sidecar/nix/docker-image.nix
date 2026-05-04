{ pkgs, version, adversary-exe, cardano-cli, ... }:
let
  # Make sure /usr/bin/env is available in the image
  usrBinEnv = pkgs.runCommand "usr-bin-env" { } ''
    mkdir -p $out/usr/bin
    ln -s ${pkgs.coreutils}/bin/env $out/usr/bin/env
  '';
  sidecar =
    pkgs.writeShellScriptBin "sidecar" (builtins.readFile ../sidecar.sh);
in pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/cardano-node-antithesis/sidecar";
  tag = version;
  config = { EntryPoint = [ "sidecar" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [
      pkgs.coreutils
      pkgs.bash
      pkgs.jq
      usrBinEnv
      adversary-exe
      cardano-cli
      sidecar
    ];
  };
}
