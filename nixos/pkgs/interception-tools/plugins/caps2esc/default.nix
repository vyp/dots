with import <nixpkgs> {};

let
  version = "0.1.0";
  name = "interception-tools-caps2esc-${version}";
in stdenv.mkDerivation {
  inherit name;

  src = fetchurl {
    url = "https://gitlab.com/interception/linux/plugins/caps2esc/repository/v${version}/archive.tar.gz";
    sha256 = "1fdxqp54gwsrm2c63168l256nfwdk4mvgr7nlwdv62wd3l7zzrg8";
  };

  buildInputs = [ cmake ];

  meta = with stdenv.lib; {
    homepage = "https://gitlab.com/interception/linux/plugins/caps2esc";
    description = "Transforming the most useless key ever into the most useful one";
    license = licenses.mit;
    maintainers = with maintainers; [ vyp ];
    platforms = platforms.linux;
  };
}
