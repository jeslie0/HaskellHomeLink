{ self, pkgs, packageName, ghcVersion, nix-filter, web, ... }:
let
  armv7 = rec {
    filteredSrc =
      nix-filter {
        root =
          self;

        include =
          [ "cabal.project" "software/libs" "software/proto" "software/src" ];

        exclude = [ ];
      };

    HomeArmV7 =
      ((pkgs.pkgsCross.armv7l-hf-multiplatform.haskell-nix.project' {
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

    StrippedHomeArmV7 =
      pkgs.stdenv.mkDerivation {
        name =
          "home-armv7-stripped";

        src = nix-filter {
          root = ./.;
          include = [];
        };

        installPhase = ''
          mkdir $out;
          cp -r ${HomeArmV7}/* $out
          chmod -R u+w $out
          runHook postInstall
        '';

        postInstall = ''
          remove-references-to -t /nix/store/*-warp-lib-warp-armv7l-* $out/bin/Home
       '';

        nativeBuildInputs =
          [ pkgs.removeReferencesTo ];
      };

    HomeArmV7DockerImage =
      pkgs.dockerTools.buildImage {
        name =
          packageName;

        tag =
          HomeArmV7.version;

        architecture =
          "arm";

        copyToRoot = with pkgs.dockerTools; [
          web
          caCertificates
        ];

        config = {
          Cmd =
            [ "${StrippedHomeArmV7}/bin/Home" "+RTS" "-N4" "-RTS" ];
        };
      };
  };
in
{
  inherit armv7;
}
