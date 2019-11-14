with import <nixpkgs> {};

stdenv.mkDerivation rec {
  pname = "testoasterror";
  version = "unstable-2019-05-12";

  src = fetchFromGitHub {
    owner = "cylgom";
    repo = pname;
    rev = "71620b47872b5535f87c908883576d73153a6911";
    sha256 = "0p0l5yzfs5rcbr92m8p7vdg3qv85x1yz7alizcyqpdvvx7a3hxal";
  };

  installPhase = ''
    mkdir -p $out/lib
    mkdir -p $out/include

    ar rcs testoasterror.a obj/src/testoasterror.o
    cp testoasterror.a $out/lib
    cp src/testoasterror.h $out/include
  '';

  meta = with stdenv.lib; {
    description = "Goat-based minimalistic C unit testing library";
    homepage = "https://github.com/cylgom/testoasterror";
    license = licenses.wtfpl;
    maintainers = with maintainers; [ vyp ];
    platforms = platforms.linux;
  };
}
