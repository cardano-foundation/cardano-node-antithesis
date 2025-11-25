{
  description = "Cardano node test assets for Antithesis platform";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
    mkdocs.url = "github:paolino/dev-assets?dir=mkdocs";
    asciinema.url = "github:paolino/dev-assets?dir=asciinema";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-utils,
      mkdocs,
      asciinema,
      ...
    }:
    let

      lib = nixpkgs.lib;
      version = self.dirtyShortRev or self.shortRev;

      perSystem =
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          devShells.default = pkgs.mkShell {
            inherit system;
            buildInputs = [
              pkgs.just
              pkgs.nixfmt-classic
              pkgs.shellcheck
              pkgs.mkdocs
              mkdocs.packages.${system}.mkdocs-packages
              mkdocs.packages.${system}.mkdocs-asciinema-player
              mkdocs.packages.${system}.mkdocs-markdown-callouts
              asciinema.packages.${system}.compress
              asciinema.packages.${system}.resize
              pkgs.asciinema
            ];
            shellHook = ''
              echo "Welcome to the Cardano node test assets development shell!"
              echo "Version: ${version}"
            '';
          };
        };

    in
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] perSystem;
}
