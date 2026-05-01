{ CHaP, indexState, pkgs, ... }:

let
  indexTool = { index-state = indexState; };

  # Mirror cardano-node-clients's fix-libs to make sure
  # cardano-crypto-praos, cardano-crypto-class, cardano-lmdb,
  # blockio-uring resolve their pkgconfig deps and the heavy
  # packages skip Haddock to keep build time sane.
  fix-libs = { lib, pkgs, ... }: {
    packages.cardano-crypto-praos.components.library.pkgconfig =
      lib.mkForce [ [ pkgs.libsodium-vrf ] ];
    packages.cardano-crypto-class.components.library.pkgconfig =
      lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 pkgs.libblst ] ];
    packages.cardano-lmdb.components.library.pkgconfig =
      lib.mkForce [ [ pkgs.lmdb ] ];
    packages.blockio-uring.components.library.pkgconfig =
      lib.mkForce [ [ pkgs.liburing ] ];
    packages.cardano-ledger-binary.components.library.doHaddock =
      lib.mkForce false;
    packages.plutus-core.components.library.doHaddock =
      lib.mkForce false;
    packages.plutus-ledger-api.components.library.doHaddock =
      lib.mkForce false;
    packages.plutus-tx.components.library.doHaddock =
      lib.mkForce false;
  };

  mkProject = { lib, pkgs, ... }: {
    name = "asteria-game";
    src = ./..;
    compiler-nix-name = "ghc9122";
    modules = [ fix-libs ];
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
    withHoogle = false;
    buildInputs = [
      pkgs.git
      pkgs.just
      pkgs.nixfmt-classic
      pkgs.lmdb
      pkgs.liburing
    ];
  };

in {
  devShells.default = project.shellFor shell;
  packages.asteria-game =
    project.hsPkgs.asteria-game.components.exes.asteria-game;
  packages.asteria-bootstrap =
    project.hsPkgs.asteria-game.components.exes.asteria-bootstrap;
  inherit project;
}
