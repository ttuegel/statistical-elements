{ mkDerivation, algebra, base, equational-reasoning
, ghc-typelits-knownnat, ghc-typelits-presburger, hmatrix, inline-r
, MonadRandom, open-browser, random, statistics, stdenv, vector
}:
mkDerivation {
  pname = "statistical-elements";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    algebra base equational-reasoning ghc-typelits-knownnat
    ghc-typelits-presburger hmatrix inline-r MonadRandom open-browser
    random statistics vector
  ];
  license = stdenv.lib.licenses.unfree;
}
