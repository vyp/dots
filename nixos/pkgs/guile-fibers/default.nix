with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "guile-fibers-${version}";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "wingo";
    repo = "fibers";
    rev = "v${version}";
    sha256 = "1r47m1m112kxf23xny99f0qkqsk6626iyc5jp7vzndfiyp5yskwi";
  };

  buildInputs = [ autoreconfHook pkgconfig guile texinfo ];

  autoreconfPhase = ''
    ./autogen.sh
  '';

  meta = with stdenv.lib; {
    description = "Concurrent ML-like concurrency for Guile";
    homepage = "https://github.com/wingo/fibers";
    license = licenses.lgpl3Plus;
    maintainers = with maintainers; [ vyp ];
    platforms = platforms.all;
  };
}
