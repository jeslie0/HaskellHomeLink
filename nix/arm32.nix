{nixpkgs, ghcVersion, packageName, src, system}:
let
  crossSystemName =
    "armv7l-hf-multiplatform";
    # "raspberryPi";

  crossNixpkgsFor =
    import nixpkgs {
      inherit system;
      crossSystem = nixpkgs.lib.systems.examples.${crossSystemName};
    };

  crossStaticHaskellPackages =
    crossNixpkgsFor.haskell.packages.${ghcVersion};

  oldPkg =
    crossStaticHaskellPackages.callCabal2nix packageName src { libpipewire = crossNixpkgsFor.pipewire.dev; };
in
crossNixpkgsFor.hello
# crossNixpkgsFor.haskell.lib.overrideCabal oldPkg (old: {
#   # enableSharedExecutables = false;
#   # enableSharedLibraries = false;
#   configureFlags = (old.configureFlags or [ ]) ++ [
#     "-O2"
#     # "--ghc-option=-optl=-static"
#   #   "--extra-lib-dirs=${crossNixpkgsFor.gmp6.override { withStatic = true; }}/lib"
#   #   "--extra-lib-dirs=${crossNixpkgsFor.libffi.overrideAttrs (old : {dontDisabledStatic = true;})}/lib"
#   #   "--extra-include-dirs=${crossNixpkgsFor.pipewire.dev}/include"
#   #   "--extra-include-dirs=${crossNixpkgsFor.pipewire.dev}/include/pipewire"
#   #   "--extra-lib-dirs=${crossNixpkgsFor.pipewire.dev}/lib"
#   #   "--extra-lib-dirs=${crossNixpkgsFor.pipewire}/lib"
#   ];
#   buildFlags = (old.buildFlags or [ ]) ++ [
#   ];
# })
