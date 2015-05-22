  let
    pkgs = import <nixpkgs> {};
    repo = import /home/a2/Projects/nixpkgs {};
    rhng = repo.haskell-ng;
    ghc710c = rhng.compiler.ghc7101;
    ghc710p = rhng.packages.ghc7101;
    hsEnv = repo.haskellngPackages.ghcWithPackages (hs: with hs; ([
      cabal-install
      ghc710c
      vector
      vector-algorithms
      ]
    ));
  in
    pkgs.myEnvFun {
      name = "UAO";
      buildInputs = with repo; with haskellngPackages; [
        hsEnv
        ];
      extraCmds = with repo; ''
      '';
      }
