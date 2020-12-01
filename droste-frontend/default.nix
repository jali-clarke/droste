{pkgs, purs, spago}:
let
  stdenv = pkgs.stdenv;
  spagoPkgs = import ./spago-packages.nix {inherit pkgs;};
  buildProjectOutput = spagoPkgs.mkBuildProjectOutput {
    inherit purs;
    src = ./.;
  };

  proxiedSecurityTool = stdenv.mkDerivation {
    name = "proxied-security";
    phases = "installPhase";

    installPhase = ''
      mkdir -p $out/bin
      ln -s /usr/bin/security $out/bin/security
    '';
  };
in stdenv.mkDerivation {
  name = "droste-frontend";
  src = buildProjectOutput;
  buildInputs = stdenv.lib.optionals stdenv.isDarwin [proxiedSecurityTool] ++ [
    purs
    spago
  ];

  buildPhase = ''
    export HOME=`pwd`/tmp
    cp ${./spago.dhall} ./spago.dhall
    cp ${./packages.dhall} ./packages.dhall
    spago bundle-app --no-build --no-install --to index.js
    sed -i '/require("react-dom\/server")/d' index.js
  '';

  installPhase = ''
    mkdir -p $out
    cp index.js $out/index.js
  '';
}
