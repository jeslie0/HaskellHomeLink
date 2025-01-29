{ pkgs, ghcVersion }:
pkgs.writeScriptBin "haskell-language-server-wrapper" ''
      #!${pkgs.stdenv.shell}
      ${pkgs.haskell-nix.tool ghcVersion "haskell-language-server" "latest"}/bin/haskell-language-server "$@"
    ''
