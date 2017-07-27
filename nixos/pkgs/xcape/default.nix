with import <nixpkgs> {};

let
  version = "git-2015-03-01";
  name = "xcape-${version}";
in stdenv.mkDerivation {
  inherit name;

  src = fetchgit {
    url = "https://github.com/alols/xcape";
    rev = "f3802fc086ce9d961d644a5d29ad5b650db56215";
    sha256 = "0d79riwzmjr621ss3yhxqn2q8chn3f9rvk2nnjckz5yxbifvfg9s";
  };

  nativeBuildInputs = [ pkgconfig ];

  buildInputs = [
    xorg.inputproto xorg.libX11 xorg.libXi xorg.libXtst xorg.xextproto
    xorg.xproto
  ];

  makeFlags = [ "PREFIX=$(out)" "MANDIR=/share/man/man1" ];

  meta = {
    description = "A tool to have Escape and Control on a single key.";
    homepage = "https://github.com/alols/xcape";
    license = stdenv.lib.licenses.gpl3;
    platforms = stdenv.lib.platforms.linux;
  };
}
