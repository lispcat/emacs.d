{
  description = "Documentation build environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pythonEnv = pkgs.python3.withPackages (ps: with ps; [
          mkdocs
          mkdocs-material
          mkdocs-include-markdown-plugin
          # Add other packages here
        ]);
      in
        {
          devShells.default = pkgs.mkShell {
            buildInputs = [ pythonEnv ];

            shellHook = ''
            echo "Docs environment loaded!"
          '';
          };
        });
}
