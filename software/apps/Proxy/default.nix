{ self, pkgs, packageName, ghcVersion, nix-filter, web, ... }:
let
  aarch64 = rec {
    filteredSrc =
      nix-filter {
        root =
          self;

        include =
          [ "cabal.project" "software/libs" "software/proto" "software/src" ];

        exclude = [ ];
      };

    ProxyAArch64 =
      ((pkgs.pkgsCross.aarch64-multiplatform.haskell-nix.project' {
        compiler-nix-name =
          ghcVersion;

        src =
          filteredSrc;

        modules = [{
          packages = {
            ${packageName}.components.exes.Proxy = {
              build-tools = [ pkgs.protobuf ];
              dontStrip = false;
            };
          };
        }];
      }).flake {}).packages."${packageName}:exe:Proxy";

    StrippedProxyAArch64 =
      pkgs.stdenv.mkDerivation {
        name =
          "proxy-aarch64-stripped";

        src = nix-filter {
          root = ./.;
          include = [];
        };

        installPhase = ''
          mkdir $out;
          cp -r ${ProxyAArch64}/* $out
          chmod -R u+w $out
          runHook postInstall
        '';

        postInstall = ''
          remove-references-to -t /nix/store/*-aarch64-unknown-linux-gcc-* $out/bin/Proxy
       '';

        nativeBuildInputs =
          [ pkgs.removeReferencesTo ];
      };

    ProxyAArch64DockerImage =
      pkgs.dockerTools.buildImage {
        name =
          packageName;

        tag =
          ProxyAArch64.version;

        architecture =
          "aarch64";

        copyToRoot = with pkgs.dockerTools; [
          web
          caCertificates
        ];

        config = {
          Cmd =
            [ "${ProxyAArch64}/bin/Proxy" ];
        };
      };
  };
in
{
  inherit aarch64;
}
