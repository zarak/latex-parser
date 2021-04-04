{ pkgs ? import <nixpkgs> { } }:

pkgs.haskellPackages.callPackage ./latex-parser.nix { }
