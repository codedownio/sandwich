{
  description = "Sandwich";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";
  inputs.nixpkgsMaster.url = "github:NixOS/nixpkgs/master";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, nixpkgsMaster, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        pkgsMaster = import nixpkgsMaster { inherit system; };
      in
        {
          packages = {
            inherit (pkgsMaster) node2nix;
            inherit (pkgsMaster.haskell.packages.ghc964) weeder;

            test = pkgs.writeShellScriptBin "stack-test" ''
              export NIX_PATH=nixpkgs=${pkgs.path}
              ${pkgs.stack}/bin/stack test
            '';

            nixpkgsPath = pkgs.writeShellScriptBin "nixpkgsPath.sh" "echo -n ${pkgs.path}";
          };

          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              nodejs

              gmp

              ncurses
              pcre
              pkg-config
              postgresql
              zlib

              haskell.compiler.ghc9122
              cabal-install
              hlint
            ];
          };
        });
}
