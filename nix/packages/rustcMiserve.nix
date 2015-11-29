# Please make sure to check if rustfmt still builds when updating nightly

# Non-rust dependencies needed for miserve are:
# - glib
# - freetype
# - zlib
# - X11/Xlib
{ stdenv, callPackage }:

callPackage <nixos/pkgs/development/compilers/rustc/generic.nix> {
  shortVersion = "2015-09-02";
  isRelease = false;
  forceBundledLLVM = true;
  srcRev = "cb9323ec096dbc4b3766b770309102c7e2ed9875";
  srcSha = "0d2a4pvagrcgrigf4hhr4m0h7f97nrkq6hk7cym3hr2wgvwi5k3r";

  /* Rust is bootstrapped from an earlier built version. We need
  to fetch these earlier versions, which vary per platform.
  The shapshot info you want can be found at
  https://github.com/rust-lang/rust/blob/{$shortVersion}/src/snapshots.txt
  with the set you want at the top.
  */

  snapshotHashLinux686 = "e2553bf399cd134a08ef3511a0a6ab0d7a667216";
  snapshotHashLinux64 = "7df8ba9dec63ec77b857066109d4b6250f3d222f";
  snapshotHashDarwin686 = "29750870c82a0347f8b8b735a4e2e0da26f5098d";
  snapshotHashDarwin64 = "c9f2c588238b4c6998190c3abeb33fd6164099a2";
  snapshotDate = "2015-08-11";
  snapshotRev = "1af31d4";

  patches = [ <nixos/pkgs/development/compilers/rustc/patches/remove-uneeded-git.patch> ]
    ++ stdenv.lib.optional stdenv.needsPax <nixos/pkgs/development/compilers/rustc/patches/grsec-head.patch>;
}

