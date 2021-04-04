{ mkDerivation, aeson, base, HaTeX, lib, req }:
mkDerivation {
  pname = "latex-parser";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base HaTeX req ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
