{
  description = "Ribbit development environment (gambit, guile, chicken, kawa)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        eggs = pkgs.chickenPackages.chickenEggs;
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            # Scheme compilers (SCHEME_COMPILER=gambit|guile|chicken|kawa)
            pkgs.gambit
            pkgs.guile_3_0
            pkgs.chicken
            eggs.srfi-1
            eggs.srfi-69
            pkgs.kawa
            pkgs.jdk21

            # Hosts used by the tests
            pkgs.gcc
            pkgs.python3
            pkgs.gnumake
          ];
        };
      });
}
