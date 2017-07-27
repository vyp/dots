{ config, pkgs, ... }:

{
  # Hardware Configuration
  # ======================
  imports =
    [
      /etc/nixos/hardware-configuration.modified.nix
    ];

  # Examples
  # --------
  # # GRUB:
  # boot.loader.grub.device = "/dev/sda";
  #
  # Or:
  #
  # # Systemd-boot EFI boot loader:
  # boot.loader.systemd-boot.enable = true;
  # boot.loader.efi.canTouchEfiVariables = true;

  # Wireless
  # ========
  networking.hostName = "nixos";
  networking.wireless.enable = true;

  # Internationalisation
  # ====================
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Time Zone
  # =========
  time.timeZone = "Australia/Sydney";

  # Nix Packages
  # ============
  nix.nixPath = [
    "nixpkgs=/home/u/dots/nixos/nixpkgs"
    "nixos-config=/home/u/dots/nixos/config.nix"
  ];

  # Packages
  # --------
  environment.systemPackages = with pkgs; [
    acpi
    chromium
    curl
    emacs
    evtest
    file
    git
    guile
    imv
    mediainfo
    polybar
    pythonPackages.glances
    rxvt_unicode-with-plugins
    setroot
    stow
    sxhkd
    sxiv
    udisks
    unzip
    vimHugeX
    wget
    xcape
    xlsfonts
    xorg.mkfontdir
    xorg.mkfontscale
    xorg.setxkbmap
    xorg.xbacklight
    xorg.xev
    xorg.xkill
    xorg.xmodmap
    xorg.xrdb
    xorg.xset
    xorg.xsetroot
    xst
    # (import ./pkgs/xst)
  ];

  # Shell
  # -----
  programs.zsh.enable = true;
  programs.zsh.syntaxHighlighting.enable = true;
  programs.zsh.syntaxHighlighting.highlighters = [ "main" "brackets" ];
  users.defaultUserShell = pkgs.zsh;

  # Services
  # ========
  services.openssh.enable = true;
  services.printing.enable = true;

  # X Windows
  # ---------
  services.xserver.enable = true;
  services.xserver.layout = "us";

  # Display Manager
  # ---------------
  # Put in a separate file so you don't have to do `nixos-rebuild' everytime
  # you change the session commands.
  services.xserver.displayManager.sessionCommands = ". ~/dots/x/init";
  services.xserver.displayManager.slim = {
    enable = true;
    # Removes ugly "Session: [...]" text by placing it out of screen.
    extraConfig = "session_y 110%";
    theme = pkgs.fetchFromGitHub {
      owner = "naglis";
      repo = "slim-minimal";
      rev = "65759e026e8de1f957889e81ca6faf3b8c2167a7";
      sha256 = "0ggkxgx5bdf3yvgfhs594v1h6nkjq6df4kfg5d51jpga0989c28y";
    };
  };

  # Touchpad
  # --------
  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
    horizontalScroll = true;
    horizTwoFingerScroll = true;
    vertTwoFingerScroll = true;
  };

  # Window Manager
  # --------------
  services.xserver.windowManager.herbstluftwm.enable = true;

  # Users
  # =====
  users.extraUsers.u = {
    createHome = true;
    extraGroups = [ "wheel" ];
    group = "users";
    home = "/home/u";
    isNormalUser = true;
    uid = 1000;
  };

  # The NixOS release to be compatible with for stateful data such as
  # databases.
  # TODO: This should probably go under hardware configuration.
  system.stateVersion = "17.09";
}
