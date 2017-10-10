{ mkDerivation, attoparsec, base, Chart, Chart-gtk, hmatrix, lens
, math-functions, MonadRandom, permutation, random, refined
, statistics, stdenv, text, vector
}:
mkDerivation {
  pname = "statistical-elements";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base Chart Chart-gtk hmatrix lens math-functions
    MonadRandom permutation random refined statistics text vector
  ];
  license = stdenv.lib.licenses.unfree;
}
