{ mkDerivation, aeson, base, bytestring, case-insensitive
, ghcjs-base, hspec, hspec-core, http-types, QuickCheck, stdenv
, text, unordered-containers
}:
mkDerivation {
  pname = "ghcjs-fetch";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive ghcjs-base http-types
  ];
  testHaskellDepends = [
    aeson base ghcjs-base hspec hspec-core http-types QuickCheck text
    unordered-containers
  ];
  homepage = "https://github.com/cocreature/ghcjs-fetch#readme";
  description = "GHCJS bindings for the JavaScript Fetch API";
  license = stdenv.lib.licenses.bsd3;
}
