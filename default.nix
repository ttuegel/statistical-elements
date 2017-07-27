{ mkDerivation, attoparsec, base, equational-reasoning
, ghc-typelits-knownnat, ghc-typelits-presburger, hmatrix, inline-r
, MonadRandom, open-browser, random, statistics, stdenv, text
, vector
}:
mkDerivation {
  pname = "statistical-elements";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base equational-reasoning ghc-typelits-knownnat
    ghc-typelits-presburger hmatrix inline-r MonadRandom open-browser
    random statistics text vector
  ];
  license = stdenv.lib.licenses.unfree;
}
