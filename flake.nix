{
  description = "My Haskell project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    ps-overlay.url = "github:thomashoneyman/purescript-overlay";
    mkSpagoDerivation = {
      url = "github:jeslie0/mkSpagoDerivation";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.ps-overlay.follows = "ps-overlay";
    };
    haskellNix = {
      url = "github:jeslie0/haskell.nix";
      # inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ps-overlay, mkSpagoDerivation, haskellNix }:
    let
      supportedSystems =
        [ "aarch64-linux" "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];

      forAllSystems =
        nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
          overlays = [ haskellNix.overlay
                       ps-overlay.overlays.default
                       mkSpagoDerivation.overlays.default
                     ];
        });

      ghcVersion =
        "ghc966";

      extendHaskellPackages = { haskellPackages, alsa-lib }:
        haskellPackages.extend ( hpFinal: hpPrev: {
          alsa-hs =
            hpPrev.callCabal2nix "alsa-hs" ./software/libs/alsa-hs { inherit alsa-lib; };

          minimp3-hs =
            hpPrev.callCabal2nix "minimp3-hs" ./software/libs/minimp3-hs { };
        });

      haskellPackages = system:
        extendHaskellPackages {
          haskellPackages =
            nixpkgsFor.${system}.haskell.packages.${ghcVersion};

          alsa-lib =
            nixpkgsFor.${system}.alsa-lib;
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
            let
              pkgs =
                nixpkgsFor.${system};
            in
            {
              nixpkgs =
                nixpkgsFor.${system};

              default =
                self.packages.${system}.linux;
              # (haskellPackages system).callCabal2nix (packageName system) "${self}/software/src" {};
            } // (
              import ./nix/xCompiled.nix {
                inherit ghcVersion system nixpkgs pkgs extendHaskellPackages self;


                packageName =
                  packageName system;

                webSrc =
                  "${self}/software/src/web";

                mkSpagoDerivation =
                  pkgs.mkSpagoDerivation;

                purs-unstable = pkgs.purs-unstable;

                spago-unstable = pkgs.spago-unstable;

                esbuild = pkgs.esbuild;

                buildNpmPackage = pkgs.buildNpmPackage;

                mkDerivation = pkgs.stdenv.mkDerivation;
              })
          );


        devShell =
          forAllSystems (system:
            let
              pkgs =
                nixpkgsFor.${system};

              toolsShell =
                pkgs.mkShell {
                  # The packages that the shell is for.
                  packages = with pkgs; [
                    purescript-language-server-unstable
                    cmake
                    alsa-lib
                    spago-unstable
                    purs-unstable
                    nodePackages.npm
                    nodejs
                    purs-tidy
                  ];
                };
            in

              self.packages.${system}.project.shellFor {
                withHoogle =
                  true;

                exactDeps =
                  false;

                crossPlatforms =
                  ps: [ ps.armv7l-hf-multiplatform ];

                inputsFrom =
                  [ toolsShell ];

                tools = {
                  # cabal =
                  #   "latest";

                  haskell-language-server =
                    "latest";
                };
              }
          );
      };
}
