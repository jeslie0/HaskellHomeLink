{ self
, pkgs
, ghcVersion
, packageName
, nix-filter
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
    pkgs.haskell-nix.project' {
      compiler-nix-name = ghcVersion;
      src = dir;
      modules = [{
        packages = {
          ${packageName}.components.exes.Home.build-tools =
            [ pkgs.protobuf
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
