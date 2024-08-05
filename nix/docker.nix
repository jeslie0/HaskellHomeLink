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
          buildPhase = "mkdir tmp; mkdir $out; cp -r ${raspExe}/* tmp; chmod 777 tmp/bin/${packageName}; patchelf --shrink-rpath tmp/bin/${packageName}; strip tmp/bin/${packageName}";
          buildInputs = [pkgs.patchelf];
          installPhase = "mkdir $out; mv tmp/* $out";
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
        [ "${strippedExec}/bin/AssistantPi"];
    };
  };

  inherit strippedExec;
}
