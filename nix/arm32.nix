{nixpkgs, ghcVersion, packageName, src, system}:
let
  crossSystemName =
      "armv7l-hf-multiplatform";

  crossNixpkgsFor =
    system:
    import nixpkgs {
      inherit system;
      config = { };
      crossSystem = nixpkgs.lib.systems.examples.${crossSystemName};
    };

  crossStaticHaskellPackages =
    system:
    (crossNixpkgsFor system).pkgsStatic.haskell.packages.${ghcVersion};
in
(crossStaticHaskellPackages system).callCabal2nix packageName src {}
