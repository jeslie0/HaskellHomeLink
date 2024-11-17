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

  crossArmv7l =
    ((pkgs.pkgsCross.armv7l-hf-multiplatform.haskell-nix.project' {
      compiler-nix-name = ghcVersion;
      src = dir;
      modules = [{
      }
      {
        packages = {
          ${packageName}.components.exes.Home = {
            build-tools = [ pkgs.protobuf ];
            dontStrip = false;
          };
        };
      }];
    }).flake {}).packages."${packageName}:exe:Home";

  crossArmv7lMusl =
    (haskellPackages pkgs.pkgsCross.armv7l-hf-multiplatform).pkgsMusl.callCabal2nix (packageName) ./.. {};

  crossArmv7lStatic =
    (haskellPackages pkgs.pkgsCross.armv7l-hf-multiplatform).pkgsStatic.callCabal2nix (packageName) ./.. {};
in
{
  inherit crossArmv7l crossArmv7lMusl crossArmv7lStatic;
}
