{nixpkgs, ghcVersion, packageName, src, system}:
let
  crossSystemName =
    "aarch64-multiplatform";

  crossNixpkgsFor =
    import nixpkgs {
      inherit system;
      config = { };
      crossSystem = nixpkgs.lib.systems.examples.${crossSystemName};
    };

  crossStaticHaskellPackages =
    crossNixpkgsFor.pkgsMusl.haskell.packages.${ghcVersion};

  oldPkg =
    crossStaticHaskellPackages.callCabal2nix packageName src { libpipewire = crossNixpkgsFor.pipewire; };
in
crossNixpkgsFor.pkgsMusl.haskell.lib.overrideCabal oldPkg (old: {
  # enableSharedExecutables = true;
  # enableSharedLibraries = false;
  configureFlags = (old.configureFlags or [ ]) ++ [
    # "--ghc-option=-optl=-static"
  ];
  buildFlags = (old.buildFlags or [ ]) ++ [
    # "--ghc-option=-fexternal-interpreter"
  ];
})
