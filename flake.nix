{
  description = "Droste effect web service thing";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.flake-compat.url = "github:edolstra/flake-compat";
  inputs.flake-compat.flake = false;

  inputs.easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix";
  inputs.easy-purescript-nix.flake = false;

  inputs.spago2nix.url = "github:justinwoo/spago2nix";
  inputs.spago2nix.flake = false;

  outputs = {self, nixpkgs, flake-utils, flake-compat, easy-purescript-nix, spago2nix}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        purescript-pkgs = import easy-purescript-nix {inherit pkgs;};

        purs = purescript-pkgs.purescript;
        spago = purescript-pkgs.spago;
        spago2nix-pkg = import spago2nix {inherit pkgs;};

        droste-frontend = import ./droste-frontend {inherit pkgs purs spago; spago2nix = spago2nix-pkg;};
        droste-haskell = import ./cabal-packages.nix {inherit pkgs;};
        droste = droste-haskell.overrideAttrs (oldAttrs: rec {
          src = ./.;
          preBuild = ''
            ${if builtins.hasAttr "preBuild" oldAttrs then oldAttrs.preBuild else ""}
            cp ${droste-frontend}/index.js data/index.js
          '';
        });
      in {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.cabal-install
            pkgs.ghc
            pkgs.zlib

            purs
            spago
            spago2nix-pkg
          ];
        };

        defaultPackage = droste;
      }
    );
}
