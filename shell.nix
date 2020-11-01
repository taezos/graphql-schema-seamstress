{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      morpheus-graphql-core = super.callCabal2nix "morpheus-graphql-core" (builtins.fetchGit {
        url = "gitea@taezos.org:piq9117/morpheus-graphql-core.git";
        rev = "a55cac79dce3bc3a9f61b1d3d3009e13b97e3812";
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
