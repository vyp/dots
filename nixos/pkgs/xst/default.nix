with import <nixpkgs> {};
with xorg;

stdenv.mkDerivation rec {
  name = "xst-${version}";
  version = "0.7.1";

  src = fetchFromGitHub {
    owner = "neeasade";
    repo = "xst";
    rev = "v${version}";
    sha256 = "1fh4y2w0icaij99kihl3w8j5d5b38d72afp17c81pi57f43ss6pc";
  };

  buildInputs = [
    pkgconfig libX11 ncurses libXext libXft fontconfig
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
