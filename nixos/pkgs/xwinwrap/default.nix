with import <nixpkgs> {};
with x11;

stdenv.mkDerivation rec {
  name = "xwinwrap-${version}";
  version = "2017-11-29";

  src = fetchFromGitHub {
    owner = "ujjwal96";
    repo = "xwinwrap";
    rev = "ec32e9b72539de7e1553a4f70345166107b431f7";
    sha256 = "0l3ng6w8jl9mryk5nx61qpd1bv5yqm7cnwy2s3vcy88n04ggbap9";
  };

  buildInputs = [ x11 ];

  buildPhase = ''
    gcc xwinwrap.c -g -Wall -lX11 -lXext -lXrender -o xwinwrap
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp xwinwrap $out/bin
  '';

  meta = with stdenv.lib; {
    description = "Stick apps to desktop background";
    homepage = "https://github.com/ujjwal96/xwinwrap";
    license = licenses.hpnd;
    maintainers = with maintainers; [ vyp ];
    platforms = platforms.linux;
  };
}
