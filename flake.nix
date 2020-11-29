{
  description = "Droste effect web service thing";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.flake-compat.url = "github:edolstra/flake-compat";
  inputs.flake-compat.flake = false;

  inputs.easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix";
  inputs.easy-purescript-nix.flake = false;

  outputs = {self, nixpkgs, flake-utils, flake-compat, easy-purescript-nix}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        purescript-pkgs = import easy-purescript-nix {inherit pkgs;};
      in {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.cabal-install
            pkgs.cabal2nix
            pkgs.ghc
            pkgs.zlib

            purescript-pkgs.purescript
            purescript-pkgs.spago
            purescript-pkgs.spago2nix
          ];
        };

        defaultPackage = pkgs.haskellPackages.callPackage ./nix/droste.nix {};
      }
    );
}
