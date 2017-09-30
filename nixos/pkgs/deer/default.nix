{ stdenv, fetchFromGitHub, perl }:

stdenv.mkDerivation rec {
  name = "deer-${version}";
  version = "1.4";

  src = fetchFromGitHub {
    owner = "Vifon";
    repo = "deer";
    rev = "v${version}";
    sha256 = "1xnbnbi0zk2xsyn8dqsmyxqlfnl36pb1wwibnlp0dxixw6sfymyl";
  };

  prePatch = ''
    substituteInPlace deer --replace \
      "| perl -pe " \
      "| ${perl}/bin/perl -pe "
  '';

  patches = [ ./realpath.patch ];

  installPhase = ''
    mkdir -p $out/share/zsh/site-functions/
    cp deer $out/share/zsh/site-functions/
  '';

  meta = with stdenv.lib; {
    description = "Ranger-like file navigation for zsh";
    homepage = "https://github.com/Vifon/deer";
    license = licenses.gpl3Plus;
    maintainers = with maintainers; [ vyp ];
    platforms = platforms.unix;
  };
}
