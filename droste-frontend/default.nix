{pkgs, purs}:
let
  spagoPkgs = import ./spago-packages.nix {inherit pkgs;};
  buildProjectOutput = spagoPkgs.mkBuildProjectOutput {
    inherit purs;
    src = ./.;
  };
in pkgs.stdenv.mkDerivation {
  name = "droste-frontend";
  buildInputs = [purs];
  src = buildProjectOutput;

  buildPhase = ''
    purs bundle output/**/*.js --main Main -o index.js
  '';

  installPhase = ''
    mkdir -p $out
    cp index.js $out/index.js
  '';
}
