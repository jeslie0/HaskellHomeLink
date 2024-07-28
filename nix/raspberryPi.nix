{ pkgs
, ghcVersion
, packageName
, system
, ...
}:
let
  crossRaspberryPi =
    pkgs.pkgsCross.raspberryPi.haskell-nix.project' {
      compiler-nix-name = ghcVersion;
      src = ../.;
      modules = [{
        # You will need to put build fixes here.
        buildable = false;
      }];
    };

  crossRaspberryPiMusl =
    pkgs.pkgsCross.raspberryPi.pkgsMusl.haskell-nix.project' {
      compiler-nix-name = ghcVersion;
      src = ../.;
      modules = [{
        # You will need to put build fixes here.
      }];
    };

  crossRaspberryPiStatic =
    pkgs.pkgsCross.raspberryPi.pkgsStatic.haskell-nix.project' {
      compiler-nix-name = ghcVersion;
      src = ../.;
      modules = [{
        # You will need to put build fixes here.
      }];
    };
in
{
  crossRaspberryPi =
    pkgs.pkgsCross.raspberryPi.haskell.packages.ghc965.callCabal2nix (packageName) ./.. {};

  #   (crossRaspberryPi.flake {}).packages."${packageName}:exe:${packageName}";

  crossRaspberryPiStatic =
    (crossRaspberryPiStatic.flake {}).packages."${packageName}:exe:${packageName}";

  crossRaspberryPiMusl =
    (crossRaspberryPiMusl.flake {}).packages."${packageName}:exe:${packageName}";

  pipewirePi =
    pkgs.pkgsCross.raspberryPi.pipewire;
}
