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

  crossRaspberryPi =
    # (haskellPackages pkgs.pkgsCross.raspberryPi).callCabal2nix (packageName) ./.. {};
    ((pkgs.pkgsCross.raspberryPi.haskell-nix.project' {
      compiler-nix-name = ghcVersion;
      src = dir;
      modules = [{
        reinstallableLibGhc = false;
      }];
    }).flake {}).packages."${packageName}:exe:Home";

  crossRaspberryPiMusl =
    (haskellPackages pkgs.pkgsCross.raspberryPi.pkgsMusl).callCabal2nix (packageName) dir {};

  crossRaspberryPiStatic =
    (haskellPackages pkgs.pkgsCross.raspberryPi.pkgsStatic).callCabal2nix (packageName) dir {};
in
{
  inherit crossRaspberryPi crossRaspberryPiStatic crossRaspberryPiMusl;
}
