{ nixpkgs ? import ./nix/pinned.nix {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  overridenHaskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      morpheus-graphql-core = super.callCabal2nix "morpheus-graphql-core" (builtins.fetchGit {
        url = "gitea@taezos.org:piq9117/morpheus-graphql-core.git";
        rev = "086b60fa0246796d10cd8fec097c22557be34c37";
      }){};
    };
  };

  f = { mkDerivation, ansi-terminal, base, bytestring, directory
      , filepath, hspec, lens, morpheus-graphql-core, mtl
      , optparse-applicative, relude, safe-exceptions, stdenv, text
      , unliftio, unordered-containers
      }:
      mkDerivation {
        pname = "graphql-stitch-vomit";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          ansi-terminal base bytestring directory filepath lens
          morpheus-graphql-core mtl optparse-applicative relude
          safe-exceptions text unliftio unordered-containers
        ];
        executableHaskellDepends = [ base relude ];
        enableSharedExecutables = false;
        enableSharedLibraries = false;
        configureFlags = [
          "--ghc-option=-optl=-static"
          "--ghc-option=-optl=-pthread"
          "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
          "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
          "--ghc-option=-optl=-L${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
          "--ghc-option=-optl=-L${pkgs.ncurses.override { enableStatic = true; }}/lib"
        ];
        testHaskellDepends = [
          base hspec morpheus-graphql-core mtl relude unordered-containers
        ];
        license = "Apache2.0";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                    then overridenHaskellPackages
                    else overridenHaskellPackages.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
  # NOTE: produced with `cabal2nix --shell . > default.nix`
