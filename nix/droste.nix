{ mkDerivation, aeson, base, bytestring, directory, filepath
, JuicyPixels, mtl, servant-multipart, servant-server, stdenv
, utf8-string, uuid, wai-logger, warp
}:
mkDerivation {
  pname = "droste";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring directory filepath JuicyPixels mtl
    servant-multipart servant-server utf8-string uuid
  ];
  executableHaskellDepends = [
    base directory servant-server wai-logger warp
  ];
  license = stdenv.lib.licenses.mit;
}
