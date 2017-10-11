{ config, pkgs, ... }:

{
  # Hardware Configuration
  # ======================
  imports =
    [
      ../../../../etc/nixos/hardware-configuration.modified.nix
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
    "nixos-config=/home/u/dots/nixos/config.nix"
    "nixpkgs=/home/u/dots/nixos/nixpkgs"
  ];

  nixpkgs.overlays = [
    (import ./overlays/pkgs.nix)
  ];

  # Fonts
  # -----
  fonts.fonts = with pkgs; [
    font-awesome-ttf
    noto-fonts
  ];

  # Shell
  # -----
  programs.zsh.enable = true;
  programs.zsh.syntaxHighlighting.enable = true;
  programs.zsh.syntaxHighlighting.highlighters = [ "main" "brackets" ];

  # Packages
  # --------
  environment.systemPackages = with pkgs; [
    acpi
    chromium
    compton
    curl
    deer
    emacs
    evtest
    file
    git
    guile
    haskellPackages.idris
    htop
    imv
    maim
    mediainfo
    nix-repl
    nixUnstable
    polybar
    scrot
    setroot
    stow
    sxhkd
    sxiv
    tree
    udisks
    unzip
    vimHugeX
    vimPlugins.commentary
    vimPlugins.repeat
    vimPlugins.surround
    wget
    xlsfonts
    xorg.mkfontdir
    xorg.mkfontscale
    xorg.xbacklight
    xorg.xev
    xorg.xkill
    xorg.xrdb
    xorg.xset
    xorg.xsetroot
    xst
  ];

  # Services
  # ========
  services.openssh.enable = true;
  services.printing.enable = true;

  # Interception Tools
  # ------------------
  services.interception-tools.enable = true;
  services.interception-tools.plugins =
    [ pkgs.interception-tools-plugins.personal ];
  services.interception-tools.udevmonConfig =
    ../interception-tools/udevmon.yaml;

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
  users.defaultUserShell = pkgs.zsh;
  users.extraUsers.u = {
    extraGroups = [ "wheel" ];
    isNormalUser = true;
    uid = 1000;
  };
}
