{
  inputs.nixpkgs.url = "nixpkgs";
  description = "Book Haskell from First Principles";
  outputs = inputs:
    let
      system = "x86_64-linux";
      pkgs = import inputs.nixpkgs { system = system; };
    in {
      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs = [
          (pkgs.ghc.withPackages (x:
            with x; [
              cabal-install
              ghcid
              haskell-language-server

            ]))
        ];
      };
    };
}
