{ pkgs, haskellNix, ghcVersion, system }:
pkgs.writeScriptBin "haskell-language-server-wrapper" ''
      #!${pkgs.stdenv.shell}
      ${haskellNix.legacyPackages.${system}.haskell-nix.tool ghcVersion "haskell-language-server" "latest"}/bin/haskell-language-server "$@"
    ''
