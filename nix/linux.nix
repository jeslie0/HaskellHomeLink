{ self
, pkgs
, ghcVersion
, packageName
, nix-filter
, haskellNix
, system
, ...
}:
let
  dir =
      nix-filter {
        root =
          self;

        include =
          [ "cabal.project" "software/libs" "software/proto" "software/src" ];

        exclude = [ ];
      };

  project =
    haskellNix.legacyPackages.${system}.haskell-nix.cabalProject' {
      compiler-nix-name = ghcVersion;
      src = dir;
      modules = [{
        packages = {
          ${packageName}.components.exes.Home.build-tools =
            [ pkgs.protobuf
              # haskellNix.legacyPackages.${system}.haskell-nix.haskellPackages.ghc966
              pkgs.haskellPackages.proto-lens-protoc
            ];
        };
      }];
    };

  flake =
    (project.flake {});

  linux =
    flake.packages."${packageName}:exe:Home";
in
{
  inherit linux flake project;
}
