{ pkgs
, ghcVersion
, packageName
, extendHaskellPackages
, ...
}:
let
  haskellPackages = pkgs:
    extendHaskellPackages {
      haskellPackages =
        pkgs.haskell.${ghcVersion};

      alsa-lib =
        pkgs.alsa-lib;
    };

  crossRaspberryPi =
    (haskellPackages pkgs.pkgsCross.raspberryPi).callCabal2nix (packageName) ./.. {};

  crossRaspberryPiMusl =
    (haskellPackages pkgs.pkgsCross.raspberryPi.pkgsMusl).callCabal2nix (packageName) ./.. {};

  crossRaspberryPiStatic =
    (haskellPackages pkgs.pkgsCross.raspberryPi.pkgsStatic).callCabal2nix (packageName) ./.. {};
in
{
  inherit crossRaspberryPi crossRaspberryPiStatic crossRaspberryPiMusl;
}
