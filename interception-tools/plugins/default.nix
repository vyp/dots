with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "interception-tools-plugins-${version}";
  version = "2017-10-02";

  src = ./.;

  buildPhase = ''
    gcc -o caps2leftctrl+esc caps2leftctrl+esc.c event.c
    gcc -o enter+rightmeta enter+rightmeta.c event.c
    gcc -o leftctrl+esc leftctrl+esc.c event.c
    gcc -o leftmeta+compose leftmeta+compose.c event.c
    gcc -o nocaps nocaps.c event.c
    gcc -o rightctrl+esc rightctrl+esc.c event.c
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp caps2leftctrl+esc $out/bin
    cp enter+rightmeta $out/bin
    cp leftctrl+esc $out/bin
    cp leftmeta+compose $out/bin
    cp nocaps $out/bin
    cp rightctrl+esc $out/bin
  '';
}
