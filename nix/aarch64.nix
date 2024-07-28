{ pkgs
, ghcVersion
, packageName
, ...
}:
let
  crossAarch64 =
    pkgs.pkgsCross.aarch64-multiplatform.haskell-nix.project' {
      compiler-nix-name = ghcVersion;
      src = ../.;
      modules = [{
        # You will need to put build fixes here.
      }];
    };

  crossAarch64Musl =
    pkgs.pkgsCross.aarch64-multiplatform.pkgsMusl.haskell-nix.project' {
      compiler-nix-name = ghcVersion;
      src = ../.;
      modules = [{
        # You will need to put build fixes here.
      }];
    };

  crossAarch64Static =
    pkgs.pkgsCross.aarch64-multiplatform.pkgsStatic.haskell-nix.project' {
      compiler-nix-name = ghcVersion;
      src = ../.;
      modules = [{
        # You will need to put build fixes here.
      }];
    };
in
{
  crossAarch64 =
    (crossAarch64.flake {}).packages."${packageName}:exe:${packageName}";

  crossAarch64Static =
    (crossAarch64Static.flake {}).packages."${packageName}:exe:${packageName}";

  crossAarch64Musl =
    (crossAarch64Musl.flake {}).packages."${packageName}:exe:${packageName}";
}
