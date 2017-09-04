with import <nixpkgs> {};

let
  version = "0.7.1";
  name = "xst-${version}";
in stdenv.mkDerivation {
  inherit name;

  src = fetchurl {
    url = "https://github.com/neeasade/xst/archive/v${version}.tar.gz";
    sha256 = "19ayx1753f2s6k7f6yn256bsssm20ggffs1diakgjqwcyjcxxn7q";
  };

  buildInputs = [
    pkgconfig xorg.libX11 ncurses xorg.libXext xorg.libXft fontconfig
  ];

  installPhase = ''
    TERMINFO=$out/share/terminfo make install PREFIX=$out
  '';

  meta = with stdenv.lib; {
    description = "Simple terminal fork that can load config from Xresources";
    homepage = "https://github.com/neeasade/xst";
    license = licenses.mit;
    maintainers = with maintainers; [ vyp ];
    platforms = platforms.linux;
  };
}
