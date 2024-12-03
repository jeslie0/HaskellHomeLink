{ self, nix-filter, mkSpagoDerivation, purs-unstable, spago-unstable, esbuild, buildNpmPackage, mkDerivation,... }:
let
  webSrc =
    nix-filter {
      root =
        self;

      include =
        ["software/web" "software/proto"];
    };

  dependenciesOutput =
    mkSpagoDerivation rec {
      spagoYaml =
        "${src}/spago.yaml";

      spagoLock =
        "${src}/spago.lock";

        src = nix-filter {
            root =
              "${self}/software/web";

            include =
              ["spago.yaml" "spago.lock"];
        };

      nativeBuildInputs =
        [ purs-unstable spago-unstable esbuild ];

      name =
        "web-dependencies";

      version =
        "0.1.0";

      buildPhase =
        "spago install";

      installPhase = ''
        mkdir $out
        cp -r output $out
        cp -r .spago $out
      '';
    };

  spago =
    mkSpagoDerivation {
      spagoYaml =
        "${webSrc}/software/web/spago.yaml";

      spagoLock =
        "${webSrc}/software/web/spago.lock";

      src =
        "${webSrc}/software/web";

      nativeBuildInputs =
        [ purs-unstable spago-unstable esbuild ];

      version =
        "0.1.0";

      buildPhase = ''
        cp -r ${dependenciesOutput}/output ./
        ln -s ${dependenciesOutput}/.spago .spago
        chmod -R u+w ./output
        spago build
      '';

      installPhase =
        "mkdir $out; cp -r * $out";
    };

  npmPkg =
    buildNpmPackage {
      pname = "HaskellHomeLink-web-build";
      version = "0.1.0";
      src = spago;
      npmDepsHash = "sha256-bw0qZJ9rnci3a2T9/4p0oZdgFenzJNrtX3CDXBbAMb0=";
    };
in
mkDerivation {
  pname =
    "HaskellHomeLink-Web";

  version =
    "0.1.0";

  src =
    npmPkg;

  installPhase = ''
                 mkdir -p $out/usr/local/haskell-home-link;
                 cp -r lib/node_modules/haskell-home-link/dist/* $out/usr/local/haskell-home-link
                 '';
}
