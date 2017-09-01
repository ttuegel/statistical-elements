(import ./shell.nix {}).env.overrideAttrs (attrs: {
  name = "dir-locals.el";
  phases = "buildPhase";
  buildPhase = ''
    eval $shellHook
    execPath=$(dirname ''${NIX_GHC:?})
    cat >$out <<EOF
    (setq exec-path (cons "$execPath" exec-path))
    EOF
  '';
})
