{ pkgs
, ghcVersion
, packageName
, system
, ...
}@args:
(import ./raspberryPi.nix args)
// (import ./aarch64.nix args)
// (import ./arm7.nix args)
// (import ./linux.nix args)
// (import ./docker.nix args (import ./arm7.nix args).crossArmv7l)
