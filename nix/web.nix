{webSrc, mkSpagoDerivation, purs-unstable, spago-unstable, esbuild, buildNpmPackage, mkDerivation,...}:
let spago =
      mkSpagoDerivation {
        spagoYaml = "${webSrc}/spago.yaml";
        spagoLock = "${webSrc}/spago.lock";
        src = webSrc;
        nativeBuildInputs = [ purs-unstable spago-unstable esbuild ];
        version = "0.1.0";
        buildPhase = "spago build";
        installPhase = "mkdir $out; cp -r * $out";
      };

    npmPkg =
      buildNpmPackage {
        pname = "HaskellHomeLink";
        version = "0.1.0";
        src = spago;
        npmDepsHash = "sha256-bw0qZJ9rnci3a2T9/4p0oZdgFenzJNrtX3CDXBbAMb0=";
      };
in
{
  web =
    mkDerivation {
      pname = "HaskellHomeLink-WEB";
      version = "0.1.0";
      src = npmPkg;
      installPhase = "mkdir -p $out/usr/local/haskell-home-link; cp -r lib/node_modules/haskell-home-link/dist/* $out/usr/local/haskell-home-link";
    };
}
