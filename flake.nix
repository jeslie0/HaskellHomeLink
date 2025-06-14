{
  description = "Haskell Home Link - a controller for your home (in Haskell!)";

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
          overlays = [ # haskellNix.overlay this causes GHC to be rebuilt
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
                  protobuf = pkgs.protobuf;
                  protoc-gen-purescript = purescript-protobuf.packages.${system}.protoc-gen-purescript;
                };

              HomeArmv7 =
                import ./software/apps/Home/default.nix {
                  inherit self pkgs ghcVersion web haskellNix system;

                  packageName =
                    packageName system;

                  nix-filter =
                    nix-filter.lib;
                };

              ProxyAArch64 =
                import ./software/apps/Proxy/default.nix {
                  inherit self pkgs ghcVersion web haskellNix system;

                  packageName =
                    packageName system;

                  nix-filter =
                    nix-filter.lib;
                };

              linux =
                import ./nix/linux.nix {
                  inherit self pkgs ghcVersion haskellNix system;

                  packageName =
                    packageName system;

                  nix-filter =
                    nix-filter.lib;
                };
            in {
              inherit web;

              nixpkgs =
                nixpkgsFor.${system};

              default =
                linux.linux;

              project =
                linux.project;

            } // HomeArmv7
              // ProxyAArch64
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
                    protobuf
                    purescript-protobuf.packages.${system}.protoc-gen-purescript
                    (import ./nix/hls-wrapper.nix { inherit pkgs ghcVersion haskellNix system; })
                  ];

                  inputsFrom = [ self.packages.${system}.web ];
                };
            in
              self.packages.${system}.project.shellFor {
                withHoogle =
                  true;

                exactDeps =
                  false;

                # crossPlatforms =
                #   ps: [ ps.armv7l-hf-multiplatform ps.aarch64-multiplatform ];

                inputsFrom =
                  [ toolsShell ];

                tools = {
                  haskell-language-server = {
                    src = pkgs.haskell-nix.sources."hls-2.10";
                    cabalProjectLocal = ''
                          allow-newer: haddock-library:base
                          '';
                  };
                  cabal =
                    {};
                };
              }
          );
      };
}
