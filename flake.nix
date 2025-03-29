{
  description = "A COOL compiler";

  inputs = { nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable"; };

  outputs = { self, nixpkgs }:
    let
      # User-friendly version number
      version = builtins.substring 0 8 self.lastModifiedDate;

      # System types to support
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    in {
      devShells = forAllSystems (system:
        let pkgs = nixpkgsFor.${system};
        in {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [ ghc haskell-language-server ormolu ];
          };

          oldghc =
            pkgs.mkShell { buildInputs = [ pkgs.haskell.compiler.ghc810 ]; };

          static = pkgs.mkShell { buildInputs = [ pkgs.glibc.static ]; };
        });
    };
}
