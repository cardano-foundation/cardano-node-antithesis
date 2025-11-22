{ pkgs, project, version, ... }:

pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/cardano-node-antithesis/tracer-sidecar";
  tag = version;
  config = { EntryPoint = [ "tracer-sidecar" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [
      project.packages.tracer-sidecar.package.components.exes.tracer-sidecar
    ];
  };
}
