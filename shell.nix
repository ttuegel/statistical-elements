{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;
  inherit (pkgs.haskell) lib stdenv;

  overrides =
    if builtins.pathExists ./overrides.nix
    then import ./overrides.nix pkgs pkgs.haskell.lib
    else self: super: {};

  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages.override { inherit overrides; }
    else pkgs.haskell.packages.${compiler}.override { inherit overrides; };

  drv = lib.overrideCabal (haskellPackages.callPackage ./. {}) (args: {

    src =
      let
        omitted = [ ".git" "dist" "nixpkgs" ];
        isOmitted = path: stdenv.lib.elem (baseNameOf path) omitted;
        pred = path: type: type != "directory" || !(isOmitted path);
      in
        builtins.filterSource pred args.src;

    shellHook = with pkgs.rPackages; ''
        R_LIBS_SITE=
        rPackages=
        findInputs ${ggplot2} rPackages propagated-native-build-inputs
        findInputs ${gridExtra} rPackages propagated-native-build-inputs
        findInputs ${plotly} rPackages propagated-native-build-inputs
        findInputs ${rgl} rPackages propagated-native-build-inputs
        for p in $rPackages; do
            R_LIBS_SITE="$R_LIBS_SITE''${R_LIBS_SITE:+:}$p/library"
        done
        export R_LIBS_SITE
    '';

    librarySystemDepends = (args.librarySystemDepends or []) ++ [ pkgs.R ];

  });

in

  if pkgs.lib.inNixShell then drv.env else drv
