{ CHaP, indexState, pkgs, cardano-cli, ... }:

let
  shell = { pkgs, ... }: {
    tools = {
      cabal = { index-state = indexState; };
      cabal-fmt = { index-state = indexState; };
      haskell-language-server = { index-state = indexState; };
      hoogle = { index-state = indexState; };
      fourmolu = { index-state = indexState; };
      hlint = { index-state = indexState; };
    };
    withHoogle = true;
    buildInputs = [
      pkgs.gitAndTools.git
      pkgs.just
      pkgs.nixfmt-classic

    ];
    shellHook = ''
      echo "Entering shell for tracer-sidecar development"
    '';
  };

  mkProject = ctx@{ lib, pkgs, ... }: {
    name = "tracer-sidecar";
    src = ./..;
    compiler-nix-name = "ghc984";
    shell = shell { inherit pkgs; };

    modules = [ ];
    inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
  };
  project = pkgs.haskell-nix.cabalProject' mkProject;

in {
  devShells.default = project.shell;
  packages.tracer-sidecar =
    project.hsPkgs.tracer-sidecar.components.exes.tracer-sidecar;
  inherit project;
}
