{ pkgs
, packageName
, system
, ...
}:
raspExe:
  let strippedExec =
        pkgs.stdenv.mkDerivation {
          name = "${raspExe.name}-stripped";
          src = ./.;
          installPhase = ''
            runHook preInstall
            mkdir -p $out/bin;
            cp -r ${raspExe}/bin/Home $out/bin;
            patchelf --shrink-rpath $out/bin/Home;
            runHook postInstall
          '';
          nativeBuildInputs = [pkgs.patchelf];
        };
  in
    {
  dockerImage = pkgs.dockerTools.buildImage {
    name =
      packageName;

    tag =
      "latest";

    architecture =
      "arm";

    config = {
      Cmd =
        [ "${raspExe}/bin/Home" ];
    };
  };

  inherit strippedExec;
}
