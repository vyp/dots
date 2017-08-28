with import <nixpkgs> {};

let
  version = "2017-08-28";
  pname = "interception-tools-plugins";
in stdenv.mkDerivation {
  name = "${pname}-${version}";
  src = ./.;

  buildPhase = ''
    gcc nocaps.c -o nocaps
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp nocaps $out/bin
  '';
}
