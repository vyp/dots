with import <nixpkgs> {};

let
  version = "2017-10-27";
in fetchzip {
  name = "eb-garamond12-${version}";

  url = "https://github.com/octaviopardo/EBGaramond12/archive/19f27c58d8c5a5e2e863c36eaaf6988f9de83b20.zip";
  sha256 = "1d3sk2y8glbzpsc27f8nmjgrn3khi4mr558jwm3bwfy5x2zw1idh";

  postFetch = ''
    unzip $downloadedFile
    mkdir -p $out/share/fonts/eb-garamond12
    unzip -j $downloadedFile \*.otf -d $out/share/fonts/eb-garamond12
  '';

  meta = with stdenv.lib; {
    description = "Digitization of the Garamond shown on the Egenolff-Berner specimen";
    homepage = "https://github.com/octaviopardo/EBGaramond12";
    license = licenses.ofl;
    maintainers = with maintainers; [ vyp ];
    platforms = platforms.all;
  };
}
