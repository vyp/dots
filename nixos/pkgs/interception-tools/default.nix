with import <nixpkgs> {};

let
  libyamlcppWithoutBoost = libyamlcpp.overrideAttrs (oldAttrs: rec {
    name = "libyaml-cpp-${version}";
    version = "2017-08-25";

    src = fetchFromGitHub {
      owner = "jbeder";
      repo = "yaml-cpp";
      rev = "beb44b872c07c74556314e730c6f20a00b32e8e5";
      sha256 = "1qkr3i5lin6m36w5rbimc7pjx3nx686xnjb6lw00xf67iqrl4h4m";
    };

    buildInputs = [ cmake ];
  });

  name = "interception-tools-${version}";
  version = "0.1.1";
in stdenv.mkDerivation {
  inherit name;

  src = fetchurl {
    url = "https://gitlab.com/interception/linux/tools/repository/v${version}/archive.tar.gz";
    sha256 = "14g4pphvylqdb922va322z1pbp12ap753hcf7zf9sii1ikvif83j";
  };

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ cmake libevdev libudev libyamlcppWithoutBoost ];

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
