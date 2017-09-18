{ pkgs ? import <nixpkgs> {} }:
let
  ghcjs = pkgs.haskell.packages.ghcjsHEAD;
in
  pkgs.haskell.lib.doCheck (ghcjs.callPackage ./ghcjs-fetch.nix { })
