with import <nixpkgs> {};
with xorg;

stdenv.mkDerivation rec {
  name = "sxhkd-${version}";
  version = "2017-10-7";

  src = fetchFromGitHub {
    owner = "baskerville";
    repo = "sxhkd";
    rev = "18575135809f99f80dbdefa43a840c05682fe8d8";
    sha256 = "04llcr6acmgxnq2yb2xjxd4p93fsgsslbai46s380vrccyd954z8";
  };

  buildInputs = [ asciidoc libxcb xcbutil xcbutilkeysyms xcbutilwm ];

  makeFlags = [ "PREFIX=$(out)" ];

  meta = with stdenv.lib; {
    description = "Simple X hotkey daemon";
    homepage = "https://github.com/baskerville/sxhkd";
    license = licenses.bsd2;
    maintainers = with maintainers; [ vyp ];
    platforms = platforms.linux;
  };
}
