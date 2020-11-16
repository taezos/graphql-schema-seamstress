let
  pkgs = import <nixpkgs> {};
in
import (
  pkgs.fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs";
    rev = "fb997de4bb3d7cf7681cfd03b58e07652d7ac253";
    sha256 = "1xv18kdw5zcds8w4x1fap772bfllwkb8ykh5idx84anaaj13xn6v";
  }
)
# https://github.com/NixOS/nixpkgs/archive/fb997de4bb3d7cf7681cfd03b58e07652d7ac253.tar.gz
