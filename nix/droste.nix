{ mkDerivation, base, JuicyPixels, stdenv }:
mkDerivation {
  pname = "droste";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base JuicyPixels ];
  executableHaskellDepends = [ base JuicyPixels ];
  license = stdenv.lib.licenses.mit;
}
