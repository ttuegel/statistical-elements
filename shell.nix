{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, algebra, base, equational-reasoning
      , ghc-typelits-knownnat, ghc-typelits-presburger, hmatrix, stdenv
      , vector
      }:
      mkDerivation {
        pname = "statistical-elements";
        version = "0.0.0";
        src = ./.;
        libraryHaskellDepends = [
          algebra base equational-reasoning ghc-typelits-knownnat
          ghc-typelits-presburger hmatrix vector
        ];
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
