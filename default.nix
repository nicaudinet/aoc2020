{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callCabal2nix "aoc2020" ./. {}
