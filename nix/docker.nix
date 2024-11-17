{ pkgs
, packageName
, system
, ...
}@args:
raspExe:
let strippedExec =
      pkgs.stdenv.mkDerivation {
        name = "${raspExe.name}-stripped";
        src = ./.;
        installPhase = ''
            mkdir $out;
            cp -r ${raspExe}/* $out
            chmod -R u+w $out
            runHook postInstall
          '';
        postInstall =
          ''
           remove-references-to -t /nix/store/*-warp-lib-warp-armv7l-* $out/bin/Home
          '';
        nativeBuildInputs = [pkgs.removeReferencesTo];
      };
in
{
  dockerImage = pkgs.dockerTools.buildImage {
    name =
      packageName;

    tag =
      "latest";

    # architecture =
    #   "arm";

    copyToRoot = with pkgs.dockerTools;[
      (import ./web.nix args).web
      binSh
      caCertificates
      pkgs.bashInteractive
      pkgs.coreutils
    ];

    config = {
      Cmd =
        [ "${strippedExec}/bin/Home" "+RTS" "-N4" "-RTS"];
      Env = [ "PATH=/bin:/usr/bin" ];
    };
  };

  inherit strippedExec;
}
