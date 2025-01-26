{ pkgs }:
pkgs.writeScriptBin "haskell-language-server-wrapper" ''
      #!${pkgs.stdenv.shell}
      ${pkgs.haskell-nix.tool "ghc927" "haskell-language-server" "latest"}/bin/haskell-language-server "$@"
    ''
