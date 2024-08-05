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

  crossAarch64 =
    (haskellPackages pkgs.pkgsCross.aarch64-multiplatform).callCabal2nix (packageName) ./.. {};

  crossAarch64Musl =
    (haskellPackages pkgs.pkgsCross.aarch64-multiplatform).pkgsMusl.callCabal2nix (packageName) ./.. {};

  crossAarch64Static =
    (haskellPackages pkgs.pkgsCross.aarch64-multiplatform).pkgsStatic.callCabal2nix (packageName) ./.. {};
in
{
  inherit crossAarch64 crossAarch64Musl crossAarch64Static;
}
