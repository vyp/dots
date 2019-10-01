self: super:

{
  # deer = import ../pkgs/deer;

  # eb-garamond12 = import ../pkgs/eb-garamond12;

  # guile-fibers = import ../pkgs/guile-fibers;

  interception-tools = import ../pkgs/interception-tools;

  interception-tools-plugins = {
    caps2esc = import ../pkgs/interception-tools/caps2esc.nix;
    personal = import ../../interception-tools/plugins;
  };

  # mytexlive = (super.pkgs.texlive.combine {
  #   inherit (super.pkgs.texlive)
  #   collection-bibtexextra
  #   collection-binextra
  #   collection-fontsextra
  #   collection-fontsrecommended
  #   collection-latexrecommended
  #   collection-luatex
  #   collection-mathextra
  #   collection-pictures
  #   collection-publishers
  #   collection-science
  #   etoolbox
  #   filehook;
  # });

  # otfcc = import ../pkgs/otfcc;

  # setroot = import ../pkgs/setroot;

  # sxhkd = import ../pkgs/sxhkd;

  # xst = import ../pkgs/xst;
}
