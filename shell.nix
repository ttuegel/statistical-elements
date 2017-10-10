{ nixpkgs ? import ./nixpkgs {}, compiler ? "default" }:

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

  });

in

  if pkgs.lib.inNixShell then drv.env else drv
