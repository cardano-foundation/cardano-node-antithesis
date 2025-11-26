{
  description = "tracer-sidecar docker image for Cardano node test assets";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, haskellNix, ... }:
    let
      version = self.dirtyShortRev or self.shortRev;
      parts = flake-parts.lib.mkFlake { inherit inputs; } {
        systems = [ "x86_64-linux" "aarch64-darwin" ];
        perSystem = { system, ... }:
        let
          pkgs = import nixpkgs {
            overlays = [ haskellNix.overlay ];
            inherit system;
          };
          project = pkgs.callPackage ./nix/project.nix {
            indexState = "2025-08-07T00:00:00Z";
          };
          docker-image = pkgs.callPackage ./nix/docker-image.nix {
            inherit project version;
          };
        in rec {
          packages = {
            inherit (project.packages) tracer-sidecar;
            inherit docker-image;
            default = packages.tracer-sidecar;
          };
          inherit (project) devShells;
         };
      };
    in {
      inherit (parts) packages devShells;
      inherit version;
    };

}
