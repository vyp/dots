{ lib, stdenv, fetchFromGitHub, meson, ninja }:

stdenv.mkDerivation rec {
  pname = "janet";
  version = "1.17.1";

  src = fetchFromGitHub {
    owner = "janet-lang";
    repo = pname;
    rev = "v${version}";
    sha256 = "4ddDJjsTCK7kUl5rH9K644cyB9it7vma1q7566//Y2c=";
  };

  postPatch = ''
    substituteInPlace janet.1 \
      --replace /usr/local/lib/janet $out/lib
  '';

  nativeBuildInputs = [ meson ninja ];

  doCheck = true;

  meta = with lib; {
    description = "Janet programming language";
    homepage = "https://janet-lang.org/";
    license = licenses.mit;
    maintainers = with maintainers; [ vyp ];
    platforms = platforms.all;
  };
}
