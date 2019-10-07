self: super:

{
  # deer = import ../pkgs/deer;

  # eb-garamond12 = import ../pkgs/eb-garamond12;

  # guile-fibers = import ../pkgs/guile-fibers;

  emacs = self.emacs27;

  emacs27 = with self; stdenv.lib.overrideDerivation
    (self.emacs26.override { srcRepo = true; }) (attrs: rec {
      name = "emacs-${version}${versionModifier}";
      version = "27.0";
      versionModifier = ".50";
      src = fetchGit {
        url = "git://git.sv.gnu.org/emacs.git";
        rev = "9d829b8be5b86668d5165b9d0c0cdc392b558dd3";
      };

      patches = [];
      buildInputs = super.emacs.buildInputs ++
        [ super.jansson super.harfbuzz.dev super.cairo ];
      configureFlags = super.emacs.configureFlags ++ [ "--with-cairo" ];
    });

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
