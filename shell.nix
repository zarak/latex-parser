let
  config = 
  {
    packageOverrides = pkgs: 
    {
      haskellPackages = pkgs.haskellPackages.override 
      {
        overrides = new: old:
        {
          latex-parser = new.callPackage ./latex-parser.nix;
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

  ghc = pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs;
    [ cabal-install
      latex-parser
      zlib
    ]
  );
in

with pkgs;

mkShell 
{ buildInputs = [ ghc ];
}
