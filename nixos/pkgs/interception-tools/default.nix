with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "interception-tools-${version}";
  version = "0.1.1";

  src = fetchurl {
    url = "https://gitlab.com/interception/linux/tools/repository/v${version}/archive.tar.gz";
    sha256 = "14g4pphvylqdb922va322z1pbp12ap753hcf7zf9sii1ikvif83j";
  };

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ cmake libevdev libudev libyamlcpp ];

  prePatch = ''
    substituteInPlace CMakeLists.txt --replace \
      "\"/usr/include/libevdev-1.0\"" \
      "\"$(pkg-config --cflags libevdev | cut -c 3-)\""
  '';

  patches = [ ./fix-udevmon-configuration-job-path.patch ];

  meta = with stdenv.lib; {
    description = "A minimal composable infrastructure on top of libudev and libevdev";
    homepage = "https://gitlab.com/interception/linux/tools";
    license = licenses.gpl3;
    maintainers = with maintainers; [ vyp ];
    platforms = platforms.linux;
  };
}
