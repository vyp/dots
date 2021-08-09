{ stdenv
, lib
, rustPlatform
, fetchFromGitHub
, pkg-config
, dbus
, openssl
}:

rustPlatform.buildRustPackage rec {
  pname = "tiny-git";
  version = "0.9.0";

  src = fetchFromGitHub {
    owner = "osa1";
    repo = "tiny";
    rev = "b77c2230bc2ed8b9ffd1ca9c622a0cd0d985cb42";
    sha256 = "wRlGzXhq7WDsH2M/bSEHFRTRIAgLr/j5xMGSaUTjhi8=";
  };

  cargoSha256 = "0vcOSHQ67UxsCJtUB6PsnwM072HBxdTGxGwHu6KRokU=";

  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ dbus openssl ];

  meta = with lib; {
    description = "A console IRC client";
    homepage = "https://github.com/osa1/tiny";
    license = licenses.mit;
    maintainers = with maintainers; [ vyp ];
  };
}
