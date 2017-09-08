{ mkDerivation, attoparsec, base, hmatrix, inline-r, MonadRandom
, open-browser, permutation, random, statistics, stdenv, text
, vector
}:
mkDerivation {
  pname = "statistical-elements";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base hmatrix inline-r MonadRandom open-browser
    permutation random statistics text vector
  ];
  license = stdenv.lib.licenses.unfree;
}
