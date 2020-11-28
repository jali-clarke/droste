{
  description = "Droste effect web service thing";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.flake-compat.url = "github:edolstra/flake-compat";
  inputs.flake-compat.flake = false;

  outputs = {self, nixpkgs, flake-utils, flake-compat}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.cabal-install
            pkgs.cabal2nix
            pkgs.ghc
          ];
        };
      }
    );
}
