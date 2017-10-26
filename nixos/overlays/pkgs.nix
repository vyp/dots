self: super:

{
  deer = import ../pkgs/deer;

  guile-fibers = import ../pkgs/guile-fibers;

  interception-tools = import ../pkgs/interception-tools;

  interception-tools-plugins = {
    caps2esc = import ../pkgs/interception-tools/caps2esc.nix;
    personal = import ../../interception-tools/plugins;
  };

  mytexlive = (super.pkgs.texlive.combine {
    inherit (super.pkgs.texlive)
    collection-bibtexextra
    collection-binextra
    collection-fontsextra
    collection-fontsrecommended
    collection-games
    collection-humanities
    collection-langarabic
    collection-langchinese
    collection-langcjk
    collection-langenglish
    collection-langfrench
    collection-langjapanese
    collection-langkorean
    collection-langother
    collection-langpolish
    collection-langportuguese
    collection-langspanish
    collection-latexrecommended
    collection-luatex
    collection-mathextra
    collection-pictures
    collection-publishers
    collection-science;
  });

  setroot = import ../pkgs/setroot;

  sxhkd = import ../pkgs/sxhkd;

  xst = import ../pkgs/xst;
}
