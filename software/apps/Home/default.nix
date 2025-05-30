{ self
, pkgs
, packageName
, ghcVersion
, nix-filter
, system
, haskellNix
, ... }:
let
  filteredSrc =
    nix-filter {
      root =
        self;

      include =
        [ "cabal.project" "software/libs" "software/proto" "software/src" ];

      exclude = [ ];
    };

  mkHome = haskell-nix:
    ((haskell-nix.project' {
      compiler-nix-name =
        ghcVersion;

      src =
        filteredSrc;

      modules = [{
        packages = {
          ${packageName}.components.exes.Home = {
            build-tools = [ pkgs.protobuf ];
            dontStrip = false;
          };
        };
      }];
    }).flake {}).packages."${packageName}:exe:Home";

  mkStripped = { name, pkg, postInstall ? "" }:
    pkgs.stdenv.mkDerivation {
      inherit name postInstall;

      src = nix-filter {
        root = ./.;
        include = [];
      };

      installPhase = ''
          mkdir $out;
          cp -r ${pkg}/* $out
          chmod -R u+w $out
          runHook postInstall
        '';

      nativeBuildInputs =
        [ pkgs.removeReferencesTo ];
    };

  mkImage = { tag, architecture, pkg }:
    pkgs.dockerTools.buildImage {
      inherit tag architecture;
      name =
        packageName;

      copyToRoot = with pkgs.dockerTools; [
        caCertificates
      ];

      config = {
        Cmd =
          [ "${pkg}/bin/Home" "--config=/mnt/home_config.json" "+RTS" "-N4" "-RTS" ];
      };
    };

  armv7 = rec {
    home =
      mkHome haskellNix.legacyPackages.${system}.pkgsCross.armv7l-hf-multiplatform.haskell-nix;

    strippedHome =
      mkStripped {
        name = "home-armv7-stripped";
        pkg = home;
        # We aren't using warp on the home server anymore
        # postInstall = "remove-references-to -t /nix/store/*-warp-lib-warp-armv7l-* $out/bin/Home";
      };

    homeDockerImage =
      mkImage { tag = home.version; architecture = "arm"; pkg = strippedHome; };
  };

  x86_64-linux = rec {
    home =
      mkHome haskellNix.legacyPackages.${system}.haskell-nix;

    strippedHome =
      mkStripped {
        name = "home-x86_64-stripped";
        pkg = home;
      };

    homeDockerImage =
      mkImage { tag = home.version; architecture = "x86_64-linux"; pkg = strippedHome; };
  };
in
{
  inherit armv7 x86_64-linux;
}
