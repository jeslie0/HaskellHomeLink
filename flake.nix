{
  description = "My Haskell project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems =
        [ "aarch64-linux" "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];

      forAllSystems =
        nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
        });

      haskellPackages = system:
        nixpkgsFor.${system}.haskell.packages.ghc965;

      packageName = system: with builtins;
        let
          cabalFileName =
            let
              cabalFiles =
                ((filter ((nixpkgsFor.${system}).lib.hasSuffix ".cabal")) (attrNames (readDir ./.)));
            in
              head cabalFiles;

          matches =
            (match "^.*name\:\ *([^[:space:]]*).*$" (readFile "${./.}\/${cabalFileName}"));
        in
          head matches;
      in
        {
          packages =
            forAllSystems (system:
              let
                pkgs =
                  nixpkgsFor.${system};
              in
                {
                  default =
                    (haskellPackages system).callCabal2nix (packageName system) self { libpipewire = pkgs.pipewire; };

                  arm32 =
                    import ./nix/arm32.nix { inherit nixpkgs system; ghcVersion = "ghc965"; packageName = packageName system; src = ./.; };

                  arm64 =
                    import ./nix/arm64.nix { inherit nixpkgs system; ghcVersion = "ghc965"; packageName = packageName system; src = ./.; };
                }
            );


          devShell =
            forAllSystems (system:
              let
                pkgs =
                  nixpkgsFor.${system};
              in
                (haskellPackages system).shellFor {
                  # The packages that the shell is for.
                  packages = p: [
                    self.packages.${system}.default
                  ];

                  buildInputs = with (haskellPackages system);
                    [ haskell-language-server
                      cabal-install
                    ];

                  # Add build inputs of the following derivations.
                  inputsFrom = [ ];

                  # Enables Hoogle for the builtin packages.
                  withHoogle = true;
                }
            );
        };
}
