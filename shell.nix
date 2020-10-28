{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      graphql = super.callCabal2nix "graphql" (builtins.fetchGit {
        url = "git@github.com:caraus-ecms/graphql.git";
        rev = "7c0b0ace4dacbb581669f88b83b9643a83fc797a";
      }){};
    };
  };

  project = haskellPackages.callPackage ./default.nix {};
in
pkgs.stdenv.mkDerivation {
  name = "graphql-stitch-vomit";
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.ghc
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.hspec-discover
  ];
}
