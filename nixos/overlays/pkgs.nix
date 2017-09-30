self: super:

{
  deer = super.callPackage ../pkgs/deer { };

  guile-fibers = super.callPackage ../pkgs/guile-fibers { };

  interception-tools = super.callPackage ../pkgs/interception-tools { };

  interception-tools-plugins = {
    caps2esc = super.callPackage ../pkgs/interception-tools/caps2esc.nix { };
  };

  setroot = super.callPackage ../pkgs/setroot { };

  xst = super.callPackage ../pkgs/xst { };
}
