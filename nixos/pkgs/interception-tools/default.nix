with import <nixpkgs> {};

let
  version = "0.1.0";
  baseName = "interception-tools";
in stdenv.mkDerivation {
  name = "${baseName}-${version}";

  src = fetchurl {
    url = "https://gitlab.com/interception/linux/tools/repository/v${version}/archive.tar.gz";
    sha256 = "0xyr7w2r5bcy1kmfqlbw7c9rvi7ia9lcsa3851dpm1k99hf523vr";
  };

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ cmake libevdev libudev libyamlcpp ];

  prePatch = ''
    sed -i 's/"\/usr\/include\/libevdev-1.0"/"'\
    "$(pkg-config --cflags libevdev \
    | cut -c 3- \
    | sed 's/\//\\\//g')"'"/g' \
    CMakeLists.txt
  '';

  meta = {
    homepage = "https://gitlab.com/interception/linux/tools";
    description = "A minimal composable infrastructure on top of libudev and libevdev";
    license = stdenv.lib.licenses.gpl3;
    platforms = stdenv.lib.platforms.linux;
  };
}
