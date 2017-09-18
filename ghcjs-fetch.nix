{ mkDerivation, base, ghcjs-base, hspec, QuickCheck, stdenv }:
mkDerivation {
  pname = "ghcjs-fetch";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ghcjs-base ];
  testHaskellDepends = [ base hspec QuickCheck ];
  homepage = "https://github.com/cocreature/ghcjs-fetch#readme";
  license = stdenv.lib.licenses.bsd3;
}
