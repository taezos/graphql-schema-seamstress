{ mkDerivation, base, bytestring, graphql, relude, stdenv, text, megaparsec }:
mkDerivation {
  pname = "graphql-stitch-vomit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring graphql relude text megaparsec ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
