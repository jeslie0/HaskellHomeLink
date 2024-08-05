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
        pkgs.haskell.packages.${ghcVersion};

      alsa-lib =
        pkgs.alsa-lib;
    };

  crossArmv7l =
    (haskellPackages pkgs.pkgsCross.armv7l-hf-multiplatform).callCabal2nix (packageName) ./.. {};

  crossArmv7lMusl =
    (haskellPackages pkgs.pkgsCross.armv7l-hf-multiplatform).pkgsMusl.callCabal2nix (packageName) ./.. {};

  crossArmv7lStatic =
    (haskellPackages pkgs.pkgsCross.armv7l-hf-multiplatform).pkgsStatic.callCabal2nix (packageName) ./.. {};
in
{
  inherit crossArmv7l crossArmv7lMusl crossArmv7lStatic;
}
