{ pkgs ? import <nixpkgs> {} }:

{
  website = websipkgs.callPackage ./website.nix {};
  f1multiviewer = pkgs.callPackage ./f1multiviewer.nix {};
}
