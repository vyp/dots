{ config, pkgs, lib, flakeInputs, deviceName, ... }:

{
  # Hardware Configuration
  # ======================
  imports = [ (./devices + "/${deviceName}.nix") ];

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
  # networking.hostName = "nixos";
  # networking.wireless.enable = true;

  # Internationalisation
  # ====================
  console.font = "Lat2-Terminus16";
  console.keyMap = "us";
  i18n.defaultLocale = "en_US.UTF-8";

  # Time Zone
  # =========
  time.timeZone = "Australia/Sydney";

  # Nix Packages
  # ============
  # nix.nixPath = [
  #   "nixos-config=/home/u/dots/nixos/config.nix"
  #   "nixpkgs=/home/u/nixpkgs"
  # ];

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
  nix.package = pkgs.nixUnstable;
  # Pin nixpkgs to the version that built the system so that for example, `nix
  # shell nixpkgs#<package>` will likely work more efficiently.
  nix.registry.nixpkgs.flake = flakeInputs.nixpkgs;
  nix.useSandbox = true;

  # nixpkgs.overlays = [
  #   (import ./overlays/pkgs.nix)
  # ];

  # Fonts
  # -----
  fonts.fonts = with pkgs; [
    # Might be from overlays.
    # eb-garamond12
    inconsolata
    iosevka
    lato
    noto-fonts
    noto-fonts-emoji
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
    alacritty
    aria
    bibutils
    # chromium
    compton
    curl
    # deer
    emacs
    entr
    escrotum
    evtest
    fd
    ffmpeg
    file
    firefox
    git
    ghc
    gnumake
    gnupg
    guile
    herbstluftwm
    heroku
    htop
    idris
    imagemagick
    imv
    irssi
    lm_sensors
    maim
    mediainfo
    mplayer
    mpv
    # mytexlive
    next
    nix-prefetch-github
    # nixUnstable
    ormolu
    # p7zip # abandoned
    polybar
    pqiv
    purescript
    racket
    renpy
    scrot
    setroot
    # stack
    stow
    streamlink
    sxhkd
    sxiv
    syncthing
    termite
    tor-browser-bundle-bin
    tree
    udiskie
    udisks
    unar
    ungoogled-chromium
    unzip
    vimHugeX
    vimPlugins.commentary
    vimPlugins.repeat
    vimPlugins.surround
    wget
    wl-clipboard
    xlsfonts
    xorg.mkfontdir
    xorg.mkfontscale
    xorg.xbacklight
    xorg.xev
    xorg.xkill
    xorg.xrdb
    xorg.xset
    xorg.xsetroot
    xsel
    xst
    xvkbd
    xwinwrap
    yarn
    youtube-dl
    zathura
  ];

  # Services
  # ========
  services.openssh.enable = true;
  programs.ssh.askPassword = "";
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Interception Tools
  # ------------------
  # services.interception-tools.enable = true;
  # services.interception-tools.plugins =
  #   [ pkgs.interception-tools-plugins.personal ];
  # services.interception-tools.udevmonConfig =
  #   ../interception-tools/udevmon.yaml;

  # X Windows
  # ---------
  # services.xserver.enable = true;
  # services.xserver.layout = "us";

  # Display Manager
  # ---------------
  # services.xserver.displayManager.startx.enable = true;

  # Put in a separate file so you don't have to do `nixos-rebuild' everytime
  # you change the session commands.
  # services.xserver.displayManager.sessionCommands = ". ~/dots/x/init";
  # services.xserver.displayManager.slim = {
  #   enable = true;
  #   # Removes ugly "Session: [...]" text by placing it out of screen.
  #   extraConfig = "session_y 110%";
  #   theme = pkgs.fetchFromGitHub {
  #     owner = "naglis";
  #     repo = "slim-minimal";
  #     rev = "65759e026e8de1f957889e81ca6faf3b8c2167a7";
  #     sha256 = "0ggkxgx5bdf3yvgfhs594v1h6nkjq6df4kfg5d51jpga0989c28y";
  #   };
  # };

  # Touchpad
  # --------
  # services.xserver.synaptics = {
  #   enable = true;
  #   twoFingerScroll = true;
  #   horizontalScroll = true;
  #   horizTwoFingerScroll = true;
  #   vertTwoFingerScroll = true;
  # };

  # Window Manager
  # --------------
  # services.xserver.windowManager.herbstluftwm.enable = true;
  programs.sway.enable = true;

  # Users
  # =====
  users.defaultUserShell = pkgs.zsh;
  users.users.u = {
    extraGroups = [ "wheel" ];
    isNormalUser = true;
    uid = 1000;
  };
}
