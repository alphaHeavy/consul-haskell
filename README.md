[![Build Status](https://travis-ci.org/alphaHeavy/consul-haskell.svg?branch=master)](https://travis-ci.org/alphaHeavy/consul-haskell)

# `consul-haskell` [package](https://hackage.haskell.org/package/consul-haskell): A consul client for Haskell

This package is a work-in-progress, but much of the functionality works.

It was originally made by [Alpha Heavy Industries](https://github.com/alphaHeavy/) and maintainance is helped by @nh2 with partial time sponsoring from [FP Complete](https://tech.fpcomplete.com/haskell).

## Testing

`stack test`; you need to have `consul` on `PATH`.

If you don't want to care about such dependencies, install [Nix](https://nixos.org/nix/), set up [NUR](https://github.com/nix-community/NUR) (see [`shell.nix`](shell.nix)) and run:

```bash
stack --nix-shell-file shell.nix test
```

which will provide all needed programs for you.
