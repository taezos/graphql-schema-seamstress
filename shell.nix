{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      morpheus-graphql-core = super.callCabal2nix "morpheus-graphql-core" (builtins.fetchGit {
        url = "gitea@taezos.org:piq9117/morpheus-graphql-core.git";
        rev = "086b60fa0246796d10cd8fec097c22557be34c37";
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
