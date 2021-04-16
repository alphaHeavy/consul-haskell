let
  # Pin nixpkgs.
  # mpickering's NUR used below currently requires 20.03,
  # see https://github.com/mpickering/old-ghc-nix/issues/8.
  pkgs = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixos-20.09-2021-04-13";
    # Current commit from https://github.com/NixOS/nixpkgs/tree/nixos-20.03
    url = "https://github.com/nixos/nixpkgs/archive/dec334fa196a4aeedb1b60d8f7d61aa00d327499.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1sm1p2qliz11qw6va01knm0rikhpq2h4c70ci98vi4q26y4q9z72";
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
