with import <nixpkgs> {};

{
 packageOverrides = pkgs: rec {
   qutebrowser = pkgs.stdenv.lib.overrideDerivation pkgs.qutebrowser (oldAttrs: {
     src = fetchgit {
       url = "https://github.com/The-Compiler/qutebrowser.git";
       rev = "bb807cfa07640360e281c32066c38a63f406f8bd";
       sha256 = "0abki6xbn9qdgqqakygwcjlmz1k23x7x8h1v980k6zxnm424y65z";
     };
   });
 };
}
