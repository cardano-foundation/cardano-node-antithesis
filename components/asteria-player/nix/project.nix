{ CHaP, indexState, pkgs, ... }:

let
  indexTool = { index-state = indexState; };
  mkProject = ctx@{ lib, pkgs, ... }: {
    name = "asteria-player";
    src = ./..;
    compiler-nix-name = "ghc984";
    inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
  };
  project = pkgs.haskell-nix.cabalProject' mkProject;

  shell = { pkgs, ... }: {
    tools = {
      cabal = indexTool;
      cabal-fmt = indexTool;
      haskell-language-server = indexTool;
      fourmolu = indexTool;
      hlint = indexTool;
    };
    withHoogle = true;
    buildInputs = [
      pkgs.git
      pkgs.just
      pkgs.nixfmt-classic
    ];
    shellHook = ''
      echo "Entering shell for asteria-player development"
    '';
  };

in {
  devShells = {
    default = project.shellFor shell;
  };
  packages.asteria-player =
    project.hsPkgs.asteria-player.components.exes.asteria-player;
  packages.asteria-bootstrap =
    project.hsPkgs.asteria-player.components.exes.asteria-bootstrap;
  inherit project;
}
