{ mkDerivation, aeson, base, HaTeX, lib, optparse-applicative, req
, text
}:
mkDerivation {
  pname = "latex-parser";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base HaTeX optparse-applicative req text
  ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
