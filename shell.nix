let
  # Pin nixpkgs.
  # mpickering's NUR used below currently requires 20.03,
  # see https://github.com/mpickering/old-ghc-nix/issues/8.
  pkgs = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixos-20.03-2020-12-08";
    # Current commit from https://github.com/NixOS/nixpkgs/tree/nixos-20.03
    url = "https://github.com/nixos/nixpkgs/archive/030e2ce817c8e83824fb897843ff70a15c131b96.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "110kgp4x5bx44rgw55ngyhayr4s19xwy19n6qw9g01hvhdisilwf";
  }) {};

  # Needs NUR from https://github.com/nix-community/NUR
  ghc = pkgs.nur.repos.mpickering.ghc.ghc865; # Keep in sync with the GHC version defined by stack.yaml!
in
  pkgs.haskell.lib.buildStackProject {
    inherit ghc;
    name = "myEnv";

    nativeBuildInputs = with pkgs; [
      # Put libraries required to build here.
      zlib

      # Executables needed to run tests:
      consul
    ];
  }
