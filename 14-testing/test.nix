{ mkDerivation, base, hspec, lib, QuickCheck }:
mkDerivation {
  pname = "x14-testing";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base QuickCheck ];
  testHaskellDepends = [ base hspec QuickCheck ];
  doHaddock = false;
  license = lib.licenses.mit;
  mainProgram = "runner";
}
