with import <nixpkgs> {};

let
  version = "1.0.3";
  baseName = "caps2esc";
in stdenv.mkDerivation {
  name = "${baseName}-${version}";

  src = fetchFromGitHub {
    owner = "oblitum";
    repo = "caps2esc";
    rev = "v${version}";
    # sha256 = "06r2jcvxf629vn47wc8y6b4riq0id859366r32gld7yf67vs86c4";
    sha256 = "0rkjkxx8d0ch480dknywq79kh64mflqakm0flki7507p96r8y44i";
  };

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ libevdev libudev ];

  buildPhase = ''
    gcc caps2esc.c -o caps2esc $(pkg-config --cflags libevdev) -levdev -ludev
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp caps2esc $out/bin
  '';

  meta = {
    homepage = "https://github.com/oblitum/caps2esc";
    description = "Transforming the most useless key ever into the most useful one";
    license = stdenv.lib.licenses.gpl3;
    platforms = stdenv.lib.platforms.linux;
  };
}
