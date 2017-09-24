{ mkDerivation, aeson, base, bytestring, ghcjs-base, hspec
, hspec-core, http-types, QuickCheck, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "ghcjs-fetch";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring ghcjs-base http-types
  ];
  testHaskellDepends = [
    aeson base ghcjs-base hspec hspec-core http-types QuickCheck text
    unordered-containers
  ];
  homepage = "https://github.com/cocreature/ghcjs-fetch#readme";
  license = stdenv.lib.licenses.bsd3;
}
