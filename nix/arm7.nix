{ pkgs
, ghcVersion
, packageName
, nixpkgs
, system
, ...
}:
let
  crossArmv7l =
    pkgs.pkgsCross.armv7l-hf-multiplatform.haskell-nix.project' {
      compiler-nix-name = ghcVersion;
      src = ../.;
      modules = [{
        # You will need to put build fixes here.
      }];
    };

  crossArmv7lMusl =
    pkgs.pkgsCross.armv7l-hf-multiplatform.pkgsMusl.haskell-nix.project' {
      compiler-nix-name = ghcVersion;
      src = ../.;
      modules = [{
        # You will need to put build fixes here.
      }];
    };

  crossArmv7lStatic =
    pkgs.pkgsCross.armv7l-hf-multiplatform.pkgsStatic.haskell-nix.project' {
      compiler-nix-name = ghcVersion;
      src = ../.;
      modules = [{
        # You will need to put build fixes here.
      }];
    };
in
{
  crossArmv7l =
    (crossArmv7l.flake {}).packages."${packageName}:exe:${packageName}";

  crossArmv7lStatic =
    (crossArmv7lStatic.flake {}).packages."${packageName}:exe:${packageName}";

  crossArmv7lMusl =
    (crossArmv7lMusl.flake {}).packages."${packageName}:exe:${packageName}";
}
