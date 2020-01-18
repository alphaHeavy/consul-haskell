with (import <nixpkgs> {});
let
  # Needs NUR from https://github.com/nix-community/NUR
  ghc = nur.repos.mpickering.ghc.ghc865; # Keep in sync with the GHC version defined by stack.yaml!
in
  haskell.lib.buildStackProject {
    inherit ghc;
    name = "myEnv";

    nativeBuildInputs = [
      # Put libraries required to build here.
      zlib

      # Executables needed to run tests:
      consul
    ];
  }
