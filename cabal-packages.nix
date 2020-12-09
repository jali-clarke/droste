{pkgs}:
pkgs.stdenv.mkDerivation {
  name = "droste-cabal-packages";
  buildInputs = [pkgs.cabal2nix];
  src = ./.;

  buildPhase = ''
    cabal2nix . > droste-cabal-packages.nix
  '';

  installPhase = ''
    mkdir -p $out
    cp droste-cabal-packages.nix $out/droste-cabal-packages.nix
  '';
}
