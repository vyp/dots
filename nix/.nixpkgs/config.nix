with import <nixpkgs> {};

{
  packageOverrides = pkgs: rec {
    myHaskellEnv = pkgs.haskellPackages.ghcWithHoogle
      (haskellPackages: with haskellPackages; [
        xmonad xmonad-contrib
      ]);

    # qutebrowser = pkgs.stdenv.lib.overrideDerivation pkgs.qutebrowser (oldAttrs: {
    #   src = builtins.filterSource
    #     # TODO: Factor out the source directory into a variable.
    #     (path: type: (toString path) != (toString ~/sc/store/qutebrowser/.git)) ~/sc/store/qutebrowser;
    # });

    rustcLatestServo = callPackage ../packages/rustcLatestServo.nix {};
    rustcMiserve = callPackage ../packages/rustcMiserve.nix {};
    rustcNightly = callPackage ../packages/rustcNightly.nix {};
  };
}
