with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "otfcc-${version}";
  version = "0.9.6";

  src = fetchFromGitHub {
    owner = "caryll";
    repo = "otfcc";
    rev = "v${version}";
    sha256 = "1rnjfqqyc6d9nhlh8if9k37wk94mcwz4wf3k239v6idg48nrk10b";
  };

  buildInputs = [ clang gcc gnumake premake5 ];

  configurePhase = ''
    premake5 gmake
  '';

  buildPhase = ''
    cd build/gmake
    make config=release_x64
    cd -
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp bin/release-x64/libotfccdll.so $out/bin
    cp bin/release-x64/otfccbuild $out/bin
    cp bin/release-x64/otfccdump $out/bin
  '';

  meta = with stdenv.lib; {
    description = "Optimized OpenType builder and inspector";
    homepage = "https://github.com/caryll/otfcc";
    license = licenses.asl20;
    maintainers = with maintainers; [ vyp ];
    platforms = platforms.all;
  };
}
