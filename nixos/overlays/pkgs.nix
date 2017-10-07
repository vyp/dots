self: super:

{
  deer = import ../pkgs/deer;

  guile-fibers = import ../pkgs/guile-fibers;

  interception-tools = import ../pkgs/interception-tools;

  interception-tools-plugins = {
    caps2esc = import ../pkgs/interception-tools/caps2esc.nix;
    personal = import ../../interception-tools/plugins;
  };

  setroot = import ../pkgs/setroot;

  sxhkd = import ../pkgs/sxhkd;

  xst = import ../pkgs/xst;
}
