{ mkDerivation, base, bytestring, relude, stdenv, text
, directory, filepath, morpheus-graphql-core, microlens, unordered-containers
}:
mkDerivation {
  pname = "graphql-stitch-vomit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends =
    [ base bytestring relude text filepath directory morpheus-graphql-core
      microlens unordered-containers
    ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
