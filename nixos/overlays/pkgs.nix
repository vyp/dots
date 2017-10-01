self: super:

{
  deer = import ../pkgs/deer;

  guile-fibers = import ../pkgs/guile-fibers;

  interception-tools = import ../pkgs/interception-tools;

  interception-tools-plugins = {
    caps2esc = import ../pkgs/interception-tools/caps2esc.nix;
  };

  setroot = import ../pkgs/setroot;

  xst = import ../pkgs/xst;
}
