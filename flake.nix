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
      devShells = forAllSystems (system: {
        default = let pkgs = nixpkgsFor.${system};
        in pkgs.mkShell {
          buildInputs = with pkgs; [ haskell.compiler.ghc810 haskell-language-server ormolu ];
        };
      });
    };
}
