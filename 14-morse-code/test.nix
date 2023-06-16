{ mkDerivation, base, containers, lib, QuickCheck }:
mkDerivation {
  pname = "morse-code";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [ base containers ];
  executableHaskellDepends = [ base containers QuickCheck ];
  testHaskellDepends = [ base containers QuickCheck ];
  doHaddock = false;
  license = lib.licenses.mit;
  mainProgram = "morse-code";
}
