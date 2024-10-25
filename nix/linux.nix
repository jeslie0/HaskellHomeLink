{ pkgs
, ghcVersion
, packageName
, extendHaskellPackages
, self
, ...
}:
let
  dir =
    "${self}";

  haskellPackages = pkgs:
    extendHaskellPackages {
      haskellPackages =
        pkgs.haskell.packages.${ghcVersion};

      alsa-lib =
        pkgs.alsa-lib;
    };

  project =
    pkgs.haskell-nix.project' {
      compiler-nix-name = ghcVersion;
      src = dir;
      modules = [{
        packages = {
          ${packageName}.components.exes.Home.build-tools = [ pkgs.protobuf_26 pkgs.haskellPackages.proto-lens-protoc ];
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
