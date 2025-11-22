{ indexState, pkgs, ... }:

let
  indexTool = { index-state = indexState; };
  mkProject = ctx@{ lib, pkgs, ... }: {
    name = "tracer-sidecar";
    src = ./..;
    compiler-nix-name = "ghc984";

    modules = [ ];
  };
  project = pkgs.haskell-nix.cabalProject' mkProject;

  shell = { pkgs, ... }: {
    tools = {
      cabal = indexTool;
      cabal-fmt = indexTool;
      haskell-language-server = indexTool;
      hoogle = indexTool;
      fourmolu = indexTool;
      hlint = indexTool;
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

  quality-shell = { pkgs, ... }: {
    tools = {
      cabal-fmt = indexTool;
      fourmolu = indexTool;
      hlint = indexTool;
    };
    withHoogle = false;
    buildInputs = [ pkgs.nixfmt-classic pkgs.just ];
  };
in {
  devShells = {
    default = project.shellFor shell;
    quality = project.shellFor quality-shell;
  };
  packages.tracer-sidecar =
    project.hsPkgs.tracer-sidecar.components.exes.tracer-sidecar;
  packages.tracer-sidecar-tests =
    project.hsPkgs.tracer-sidecar.components.tests.test;
  inherit project;
}
