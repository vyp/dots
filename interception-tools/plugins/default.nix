with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "interception-tools-plugins-${version}";
  version = "2017-09-18";

  src = ./.;

  buildPhase = ''
    gcc -o caps2ctrl caps2ctrl.c event.c
    gcc -o nocaps nocaps.c
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp caps2ctrl $out/bin
    cp nocaps $out/bin
  '';
}
