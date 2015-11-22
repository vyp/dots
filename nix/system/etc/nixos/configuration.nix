# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  # boot.loader.grub.device = "/dev/sda";
  boot.loader.grub.device = "nodev";
  # Use the gummiboot efi boot loader.
  # boot.loader.gummiboot.enable = true;
  # boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Australia/Sydney";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  # environment.systemPackages = with pkgs; [
  #   wget
  # ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.autorun = true;
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.displayManager.lightdm.enable = true;
  # services.xserver.windowManager.i3.enable = true;
  # services.xserver.windowManager.bspwm.enable = true;
  # services.xserver.xkbOptions = "eurosign:e";

  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
    horizontalScroll = true;
    horizTwoFingerScroll = true;
    vertTwoFingerScroll = true;
  };

  services.xserver.displayManager.sessionCommands = ''
    . ~/.xinitrc
  '';

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.kdm.enable = true;
  # services.xserver.desktopManager.kde4.enable = true;
  programs.zsh.enable = true;

  users.extraUsers.root.initialPassword = "f";
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.u = {
    createHome = true;
    extraGroups = [ "wheel" ];
    group = "users";
    home = "/home/u";
    initialPassword = "f";
    isNormalUser = true;
    shell = "/run/current-system/sw/bin/zsh";
    uid = 1000;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";
}
