with import <nixpkgs> {};

let
  version = "0.1.0";
  pname = "caps2esc-interception-tools";
in stdenv.mkDerivation {
  name = "${pname}-${version}";

  src = fetchurl {
    url = "https://gitlab.com/interception/linux/plugins/caps2esc/repository/v${version}/archive.tar.gz";
    sha256 = "1fdxqp54gwsrm2c63168l256nfwdk4mvgr7nlwdv62wd3l7zzrg8";
  };

  buildInputs = [ cmake ];

  meta = {
    homepage = "https://gitlab.com/interception/linux/plugins/caps2esc";
    description = "Transforming the most useless key ever into the most useful one";
    license = stdenv.lib.licenses.mit;
    platforms = stdenv.lib.platforms.linux;
  };
}
