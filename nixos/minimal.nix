{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.modified.nix
    ];

  networking.hostName = "nixos";
  networking.wireless.enable = true;

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Australia/Sydney";

  nix.nixPath = [
    "nixos-config=/home/u/dots/nixos/config.nix"
    "nixpkgs=/home/u/dots/nixos/nixpkgs"
  ];

  users.extraUsers.u = {
    extraGroups = [ "wheel" ];
    isNormalUser = true;
    uid = 1000;
  };
}
