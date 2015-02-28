  let
    pkgs = import <nixpkgs> {};
    hsEnv = pkgs.haskellngPackages.ghcWithPackages (hs: with hs; ([
      hlint
      hdevtools
      hasktags
      vector
      vector-algorithms
      ]
    ));
  in
    pkgs.myEnvFun {
      name = "penny-models";
      buildInputs = with pkgs; with haskellngPackages; [
        # development tools as fits your needs
        cabal-install
        hsEnv
        ];
      extraCmds = with pkgs; ''
      '';
      }
