{ pkgs, project, version, imageMeta, ... }:

pkgs.dockerTools.buildImage {
  name = "ghcr.io/cardano-foundation/cardano-node-antithesis/tracer-sidecar";
  tag = version;
  created = imageMeta.created;
  config = {
    EntryPoint = [ "tracer-sidecar" ];
    Labels = imageMeta.labels;
  };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [
      project.packages.tracer-sidecar.package.components.exes.tracer-sidecar
    ];
  };
}
