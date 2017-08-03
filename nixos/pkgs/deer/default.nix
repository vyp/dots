with import <nixpkgs> {};

let
  version = "2016-06-19";
  name = "deer-${version}";
in stdenv.mkDerivation {
  inherit name;

  src = fetchFromGitHub {
    owner = "Vifon";
    repo = "deer";
    rev = "2668477e04c6978ca2cdcb1651b17f119f752ce9";
    sha256 = "02ag2dkay1rk3r3wrx65jwd7azxad4l9hszr0jlmgzyy0pl3yn59";
  };

  propagatedBuildInputs = [ perl python ];

  installPhase = ''
    mkdir -p $out/share/zsh/site-functions/
    cp deer $out/share/zsh/site-functions/
  '';

  meta = with stdenv.lib; {
    description = "Ranger-like file navigation for zsh";
    homepage = https://github.com/Vifon/deer;
    license = licenses.gpl3Plus;
    platforms = platforms.unix;
  };
}
