{pkgs}:
pkgs.stdenv.mkDerivation {
  name = "droste-cabal-packages";
  src = ./.;
  buildInputs = [pkgs.cabal2nix];

  buildPhase = ''
    cabal2nix . > droste-cabal-packages.nix
  '';

  installPhase = ''
    mkdir -p $out
    cp droste-cabal-packages.nix $out/droste-cabal-packages.nix
  '';
}
