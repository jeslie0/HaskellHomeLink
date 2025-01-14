{ self
, nix-filter
, mkSpagoDerivation
, purs-unstable
, spago-unstable
, esbuild
, buildNpmPackage
, mkDerivation
, protobuf
, protoc-gen-purescript
, ... }:
let
  webSrc =
    mkDerivation {
      name =
        "web-src";

      src =
        nix-filter {
          root =
            self;

          include =
            [ "scripts" "software/web" "software/proto"];
        };

      nativeBuildInputs = [ protobuf protoc-gen-purescript ];

      buildPhase =
        "bash scripts/compile-purescript-protos";

      installPhase =
        "mkdir $out; cp -r * $out";
    };

  dependenciesOutput =
    mkSpagoDerivation rec {
      spagoYaml =
        "${src}/spago.yaml";

      spagoLock =
        "${src}/spago.lock";

      src =
        nix-filter {
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
        mv output $out
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
        cp -r ${dependenciesOutput}/* .
        chmod -R 0755 ./output
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
      npmDepsHash = "sha256-qX5Ks7pwWcHPf/PaghfXZkwALRBr6rHh51vt8qGSQKI=";
    };

  web =
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
    };
in
web
