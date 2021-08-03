self: super:

{
  # alacritty = import ../pkgs/alacritty;

  # deer = import ../pkgs/deer;

  # eb-garamond12 = import ../pkgs/eb-garamond12;

  # guile-fibers = import ../pkgs/guile-fibers;

  # emacs = self.emacs27;

  # emacs27 = with self; stdenv.lib.overrideDerivation
  #   (self.emacs26.override { srcRepo = true; }) (attrs: rec {
  #     name = "emacs-${version}${versionModifier}";
  #     version = "27.0";
  #     versionModifier = ".91";
  #     src = fetchGit {
  #       url = "git://git.sv.gnu.org/emacs.git";
  #       rev = "c36c5a3dedbb2e0349be1b6c3b7567ea7b594f1c";
  #     };

  #     patches = [];
  #     buildInputs = super.emacs.buildInputs ++
  #       [ super.jansson super.harfbuzz.dev super.cairo ];
  #     configureFlags = super.emacs.configureFlags ++ [ "--with-cairo" ];
  #   });

  interception-tools = super.callPackage ./pkgs/interception-tools { };

  interception-tools-plugins = {
    caps2esc = super.callPackage ./pkgs/interception-tools/caps2esc.nix { };
    personal = super.callPackage ../interception-tools/plugins { };
  };

  iosevka-custom = super.callPackage ./pkgs/iosevka-custom { };

  # iosevka = super.iosevka.override {
  #   privateBuildPlan = {
  #     family = "Iosevka Custom";
  #     design = [ "v-i-hooky" "v-l-hooky" ];
  #   };
  #   set = "custom";
  # };

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
