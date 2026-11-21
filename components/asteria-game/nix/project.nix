{ CHaP, indexState, pkgs, cardano-node-clients, cardano-tx-tools, ... }:

let
  indexTool = { index-state = indexState; };

  # Mirror cardano-node-clients's fix-libs to make sure
  # cardano-crypto-praos and cardano-crypto-class
  # resolve their pkgconfig deps and the heavy
  # packages skip Haddock to keep build time sane.
  fix-libs = { lib, pkgs, ... }: {
    packages.cardano-crypto-praos.components.library.pkgconfig =
      lib.mkForce [ [ pkgs.libsodium-vrf ] ];
    packages.cardano-crypto-class.components.library.pkgconfig =
      lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 pkgs.libblst ]];
    packages.cardano-ledger-binary.components.library.doHaddock =
      lib.mkForce false;
    packages.plutus-core.components.library.doHaddock = lib.mkForce false;
    packages.plutus-ledger-api.components.library.doHaddock = lib.mkForce false;
    packages.plutus-tx.components.library.doHaddock = lib.mkForce false;
  };

  mkProject = { lib, pkgs, ... }: {
    name = "asteria-game";
    src = ./..;
    compiler-nix-name = "ghc9123";
    modules = [ fix-libs ];
    inputMap = {
      "https://chap.intersectmbo.org/" = CHaP;
      "https://github.com/lambdasistemi/cardano-node-clients/e17ffdc74c756680006d43963e337d9a766aa144" =
        cardano-node-clients;
      "https://github.com/lambdasistemi/cardano-tx-tools/2e0d7cb813a9509c6677debdfe5fdad2a76351ff" =
        cardano-tx-tools;
    };
  };
  project = pkgs.haskell-nix.cabalProject' mkProject;

  shell = { pkgs, ... }: {
    packages = ps: [ ps.asteria-game ];
    tools = {
      cabal = indexTool;
      haskell-language-server = indexTool;
    };
    withHoogle = false;
    buildInputs = [
      pkgs.git
      pkgs.just
      pkgs.nixfmt-classic
      # Cabal can rebuild public sublibraries of source dependencies.
      pkgs.lmdb
      pkgs.liburing
      pkgs.rocksdb
      pkgs.haskellPackages.cabal-fmt
      pkgs.haskellPackages.fourmolu
      pkgs.haskellPackages.hlint
    ];
  };

in {
  devShells.default = project.shellFor shell;
  # The build gate uses the same environment without the optional editor server.
  devShells.ci = project.shellFor ({ pkgs, ... }:
    (shell { inherit pkgs; }) // {
      tools = { cabal = indexTool; };
    });
  packages.asteria-game =
    project.hsPkgs.asteria-game.components.exes.asteria-game;
  packages.asteria-bootstrap =
    project.hsPkgs.asteria-game.components.exes.asteria-bootstrap;
  packages.asteria-invariant =
    project.hsPkgs.asteria-game.components.exes.asteria-invariant;
  inherit project;
}
