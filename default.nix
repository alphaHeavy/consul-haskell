# Inspired by https://discourse.nixos.org/t/nix-haskell-development-2020/6170/16
let
  pinnedPkgs = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixos-20.09-2021-04-13";
    # Current commit from https://github.com/NixOS/nixpkgs/tree/nixos-20.09
    url = "https://github.com/nixos/nixpkgs/archive/dec334fa196a4aeedb1b60d8f7d61aa00d327499.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1sm1p2qliz11qw6va01knm0rikhpq2h4c70ci98vi4q26y4q9z72";
  }) {};

  packageName = "consul-haskell";
in
{
  pkgs ? pinnedPkgs,
  # compiler:
  # If given as the default `null` uses the default `pkgs.haskellPackages`;
  # if given as a string (e.g. `ghc865`), uses
  # `pkgs.haskell.packages.${compiler}`.
  compiler ? null,
}:
let
  explicitSource =
    (import ./explicitSource.nix { lib = pkgs.lib; }).explicitSource;

  src = explicitSource ./. {
    name = "consul-haskell";
    includeDirs = [
      ./src
      ./tests
    ];
    includeFiles = [
      ./consul-haskell.cabal
      ./Setup.hs
      ./LICENSE
      ./README.md
    ];
    pathComponentExcludes = [ "build" "gen" ];
  };

  originalHaskellpackages =
    if compiler == null
      then pkgs.haskellPackages
      else pkgs.haskell.packages.${compiler};

  # Create a Haskell package set with our package in it,
  # and add the required native packages to its dependencies.
  myHaskellPackages = originalHaskellpackages.override {
    overrides = hself: hsuper: {
      "${packageName}" =
        pkgs.haskell.lib.overrideCabal
          (hself.callCabal2nix "${packageName}" src {})
          (drv: {
            # The test suite starts a consul server; add it to PATH.
            preCheck = ''
              export PATH="${pkgs.consul}/bin:$PATH"
            '';
          });
    };
  };


  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."${packageName}");

in
  {
    inherit exe;
  }
