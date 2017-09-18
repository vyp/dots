with import <nixpkgs> {};

let
  version = "2017-09-18";
  pname = "interception-tools-plugins";
in stdenv.mkDerivation {
  name = "${pname}-${version}";
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
