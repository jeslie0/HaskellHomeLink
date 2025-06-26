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

  mkCamera = haskell-nix:
    ((haskell-nix.project' {
      compiler-nix-name =
        ghcVersion;

      src =
        filteredSrc;

      modules = [{
        packages = {
          ${packageName}.components.exes.Camera = {
            build-tools = [ pkgs.protobuf
                            pkgs.haskellPackages.proto-lens-protoc
                          ];
            dontStrip = false;
          };
        };
      }];
    }).flake {}).packages."${packageName}:exe:Camera";

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

      copyToRoot = with pkgs.pkgsCross.raspberryPi; [
        libraspberrypi
        ffmpeg
      ];

      config = {
        Cmd =
          [ "${pkg}/bin/Camera" "--config=/mnt/camera_config.json" ];
      };
    };

  armv6 = rec {
    camera =
      mkCamera haskellNix.legacyPackages.${system}.pkgsCross.raspberryPi.haskell-nix;

    strippedCamera =
      mkStripped {
        name = "camera-armv6-stripped";
        pkg = camera;
        # We aren't using warp on the camera server anymore
        # postInstall = "remove-references-to -t /nix/store/*-warp-lib-warp-armv7l-* $out/bin/Camera";
      };

    cameraDockerImage =
      mkImage { tag = camera.version; architecture = "arm"; pkg = strippedCamera; };
  };

  x86_64-linux = rec {
    camera =
      mkCamera haskellNix.legacyPackages.${system}.haskell-nix;

    strippedCamera =
      mkStripped {
        name = "camera-x86_64-stripped";
        pkg = camera;
      };

    cameraDockerImage =
      mkImage { tag = camera.version; architecture = "x86_64-linux"; pkg = strippedCamera; };
  };
in
{
  inherit armv6 x86_64-linux;
}
