with import <nixpkgs> {};

let
  version = "2.0.1";
  name = "setroot-${version}";
in stdenv.mkDerivation {
  inherit name;

  src = fetchFromGitHub {
    owner = "ttzhou";
    repo = "setroot";
    rev = "v${version}";
    sha256 = "01krjfc3xpp0wbqz9nvf1n34gkpd41gysn289sj1wcjxia4n4gsi";
  };

  buildInputs = [ xorg.libX11 imlib2 xorg.libXinerama ];

  buildFlags = "CC=cc xinerama=1";

  installFlags = "DESTDIR=$(out) PREFIX=";

  meta = with stdenv.lib; {
    description = "Simple X background setter inspired by imlibsetroot and feh";
    homepage = "https://github.com/ttzhou/setroot";
    license = licenses.gpl3Plus;
    maintainers = with maintainers; [ vyp ];
    platforms = platforms.unix;
  };
}
