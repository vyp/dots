with import <nixpkgs> {};

let
  version = "2017-09-18";
  pname = "interception-tools-plugins";
in stdenv.mkDerivation {
  name = "${pname}-${version}";
  src = ./.;

  buildPhase = ''
    gcc caps2ctrl.c -o caps2ctrl
    gcc nocaps.c -o nocaps
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp caps2ctrl $out/bin
    cp nocaps $out/bin
  '';
}
