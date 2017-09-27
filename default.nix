{ mkDerivation, attoparsec, base, hmatrix, inline-r, lens
, math-functions, MonadRandom, open-browser, permutation, random
, refined, statistics, stdenv, text, vector
}:
mkDerivation {
  pname = "statistical-elements";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base hmatrix inline-r lens math-functions MonadRandom
    open-browser permutation random refined statistics text vector
  ];
  license = stdenv.lib.licenses.unfree;
}
