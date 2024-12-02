{
  description = "My Haskell project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    ps-overlay.url = "github:thomashoneyman/purescript-overlay";
    nix-filter.url = "github:numtide/nix-filter";
    mkSpagoDerivation = {
      url = "github:jeslie0/mkSpagoDerivation";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.ps-overlay.follows = "ps-overlay";
    };
    purescript-protobuf = {
      url = "github:rowtype-yoga/purescript-protobuf/610ef795387663a90ce372338aa91e29d3d5a434";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskellNix = {
      url = "github:jeslie0/haskell.nix";
    };
  };

  outputs = { self, nixpkgs, nix-filter, ps-overlay, mkSpagoDerivation, purescript-protobuf, haskellNix }:
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

              web =
                import ./software/apps/Web/default.nix {
                  inherit self;
                  nix-filter = nix-filter.lib;
                  mkSpagoDerivation = pkgs.mkSpagoDerivation;
                  purs-unstable = pkgs.purs-unstable;
                  spago-unstable = pkgs.spago-unstable;
                  esbuild = pkgs.esbuild;
                  buildNpmPackage = pkgs.buildNpmPackage;
                  mkDerivation = pkgs.stdenv.mkDerivation;
                };

              HomeArmv7 =
                import ./software/apps/Home/default.nix {
                  inherit self pkgs ghcVersion web;

                  packageName =
                    packageName system;

                  nix-filter =
                    nix-filter.lib;
                };

              linux =
                import ./nix/linux.nix {
                  inherit self pkgs ghcVersion;

                  packageName =
                    packageName system;

                  nix-filter =
                    nix-filter.lib;
                };
            in {
              nixpkgs =
                nixpkgsFor.${system};

              default =
                self.packages.${system}.linux;

              project =
                linux.project;

              web =
                web;
            } // HomeArmv7
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
                    purescript-protobuf.packages.${system}.protoc-gen-purescript
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
                  haskell-language-server =
                    "latest";
                };
              }
          );
      };
}
