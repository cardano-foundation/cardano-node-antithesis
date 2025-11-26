{
  description = "sidecar for adversary testing on Cardano nodes";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
    cardano-node-runtime = {
      url = "github:IntersectMBO/cardano-node?ref=10.1.4";
    };
    adversary.url =
      "github:cardano-foundation/cardano-node-antithesis?dir=components/adversary";
  };

  outputs =
    inputs@{ self, nixpkgs, flake-utils, cardano-node-runtime, adversary, ... }:
    let
      lib = nixpkgs.lib;
      version = self.dirtyShortRev or self.shortRev;

      perSystem = system:
        let
          node-project = cardano-node-runtime.project.${system};
          cardano-cli = node-project.pkgs.cardano-cli;
          adversary-exe = adversary.packages.${system}.adversary;
          pkgs = import nixpkgs { inherit system; };

          sidecar-image = import ./nix/docker-image.nix {
            inherit pkgs version adversary-exe cardano-cli;
          };
          docker-packages = { packages.docker-image = sidecar-image; };
          info.packages = { inherit version; };
          other-tools = { packages.cardano-cli = cardano-cli; };

          fullPackages = lib.mergeAttrsList [
            other-tools.packages
            docker-packages.packages
            info.packages
          ];
          shell = pkgs.mkShell {
            buildInputs = [
              adversary-exe
              cardano-cli
              pkgs.just
              pkgs.nixfmt-classic
              pkgs.shellcheck
            ];
          };
        in {
          devShells.default = shell;
          packages = fullPackages // { default = fullPackages.docker-image; };
        };

    in flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] perSystem;
}
