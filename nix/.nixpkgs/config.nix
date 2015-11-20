with import <nixpkgs> {};

{
 packageOverrides = pkgs: rec {
   qutebrowser = pkgs.stdenv.lib.overrideDerivation pkgs.qutebrowser (oldAttrs: {
     src = builtins.filterSource
       # TODO: Factor out the source directory into a variable.
       (path: type: (toString path) != (toString ~/gh/forks/qutebrowser/.git)) ~/gh/forks/qutebrowser;
   });
 };
}
