with import <nixpkgs> {};

stdenv.mkDerivation rec {
  pname = "testoasterror";
  version = "unstable-2018-12-03";

  src = fetchFromGitHub {
    owner = "cylgom";
    repo = pname;
    rev = "bea3bb8a2234f819682c384bb08be8fa24ff0f6b";
    sha256 = "133qzl9hj6k9xhjyah9wcs9465svmrsh8xmx1l4s788vf2sih2x9";
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
