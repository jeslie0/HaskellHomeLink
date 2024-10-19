{
  description = "My Haskell project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    ps-overlay.url = "github:thomashoneyman/purescript-overlay";
    haskellNix = {
      url = "github:jeslie0/haskell.nix";
      # inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ps-overlay, haskellNix }:
    let
      supportedSystems =
        [ "aarch64-linux" "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];

      forAllSystems =
        nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
          overlays = [ haskellNix.overlay ps-overlay.overlays.default ];
        });

      ghcVersion =
        "ghc966";

      extendHaskellPackages = { haskellPackages, alsa-lib }:
        haskellPackages.extend ( hpFinal: hpPrev: {
            alsa-hs =
              hpPrev.callCabal2nix "alsa-hs" ./libs/alsa-hs { inherit alsa-lib; };
        });

      haskellPackages = system:
        extendHaskellPackages {
          haskellPackages =
            nixpkgsFor.${system}.haskell.packages.${ghcVersion};

          alsa-lib =
            nixpkgsFor.${system}.alsa-lib.dev;
        };

      packageName = system: with builtins;
        let
          dir =
            "${self}/software/src";

          cabalFileName =
            let
              cabalFiles =
                ((filter ((nixpkgsFor.${system}).lib.hasSuffix ".cabal")) (attrNames (readDir dir)));
            in
              head cabalFiles;

          matches =
            (match "^.*name\:\ *([^[:space:]]*).*$" (readFile "${dir}\/${cabalFileName}"));
        in
          head matches;
    in
      {
        packages =
          forAllSystems (system:
            {
              nixpkgs =
                nixpkgsFor.${system};

              default =
                (haskellPackages system).callCabal2nix (packageName system) "${self}/software/src/" {};
            } // (
              import ./nix/xCompiled.nix {
                inherit ghcVersion system nixpkgs extendHaskellPackages self;

                pkgs =
                  nixpkgsFor.${system};

                packageName =
                  packageName system;
              })
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
                  [  haskell-language-server
                     pkgs.purescript-language-server-unstable
                     cabal-install
                     pkgs.cmake
                     pkgs.alsa-lib
                     pkgs.spago-unstable
                     pkgs.purs-unstable
                     pkgs.nodePackages.npm
                     pkgs.nodejs
                     pkgs.purs-tidy
                  ];


                # Add build inputs of the following derivations.
                inputsFrom = [ ];

                # Enables Hoogle for the builtin packages.
                withHoogle = true;
              }
          );
      };
}
