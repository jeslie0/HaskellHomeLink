{ pkgs
, ghcVersion
, packageName
, extendHaskellPackages
, self
, ...
}:
let
  haskellPackages = pkgs:
    extendHaskellPackages {
      haskellPackages =
        pkgs.haskell.packages.${ghcVersion};

      alsa-lib =
        pkgs.alsa-lib;
    };

  crossRaspberryPi =
    (haskellPackages pkgs.pkgsCross.raspberryPi).callCabal2nix (packageName) ./.. {};
    # ((pkgs.pkgsCross.raspberryPi.haskell-nix.project' {
    #   compiler-nix-name = ghcVersion;
    #   src = ./..;
    #   modules = [{
    #     reinstallableLibGhc = false;
    #   }];
    # }).flake {}).packages."${packageName}:exe:${packageName}";

  crossRaspberryPiMusl =
    (haskellPackages pkgs.pkgsCross.raspberryPi.pkgsMusl).callCabal2nix (packageName) ./.. {};

  crossRaspberryPiStatic =
    (haskellPackages pkgs.pkgsCross.raspberryPi.pkgsStatic).callCabal2nix (packageName) ./.. {};
in
{
  inherit crossRaspberryPi crossRaspberryPiStatic crossRaspberryPiMusl;
}
