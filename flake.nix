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

        ghcName = "ghc9124";

        compilers = {
          ghc8107 = pkgs.haskell.compiler.ghc8107;
          ghc902 = pkgs.haskell.compiler.ghc902;
          ghc967 = pkgsMaster.haskell.compiler.ghc967;
          ghc984 = pkgsMaster.haskell.compiler.ghc984;
          ghc9124 = pkgsMaster.haskell.compiler.ghc9124;
        };

        mkDevShell = ghc: pkgs.mkShell {
          buildInputs = (with pkgs; [
            nodejs

            gmp

            ncurses
            pcre
            pkg-config
            postgresql
            zlib

            stack
          ]) ++ [
            ghc
            pkgsMaster.cabal-install
            pkgsMaster.hlint
          ];
        };
      in
        {
          packages = {
            inherit (pkgsMaster) node2nix;
            inherit (pkgsMaster.haskell.packages.${ghcName}) weeder;

            test = pkgs.writeShellScriptBin "stack-test" ''
              export NIX_PATH=nixpkgs=${pkgs.path}
              ${pkgs.stack}/bin/stack test
            '';

            nixpkgsPath = pkgs.writeShellScriptBin "nixpkgsPath.sh" "echo -n ${pkgs.path}";
          };

          devShells = (builtins.mapAttrs (_: mkDevShell) compilers) // {
            default = mkDevShell compilers.${ghcName};
          };
        });
}
