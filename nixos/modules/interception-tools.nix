{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.interception-tools;
in {
  options.services.interception-tools = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to enable the interception tools service.";
    };

    plugins = mkOption {
      type = types.listOf types.package;
      default = [ (import ../pkgs/caps2esc) ];
      description = ''
        A list of interception tools plugins that will be made available to use
        inside the udevmon configuration.
      '';
    };

    # TODO: Make this /either/ a string or path to yaml file.
    udevmonConfig = mkOption {
      type = types.str;
      default = ''
        - JOB: "intercept -g $DEVNODE | caps2esc | uinput -d $DEVNODE"
          DEVICE:
            EVENTS:
              EV_KEY: [KEY_CAPSLOCK, KEY_ESC]
      '';
      example = ''
        - JOB: "intercept -g $DEVNODE | y2z | x2y | uinput -d $DEVNODE"
          DEVICE:
            EVENTS:
              EV_KEY: [KEY_X, KEY_Y]
      '';
      description = ''
        String of udevmon YAML configuration, or path to a udevmon YAML
        configuration file.
      '';
    };
  };

  config = mkIf cfg.enable {
    systemd.services.interception-tools = {
      description = ''
        Minimal composable infrastructure on top of libudev and libevdev
      '';
      path = [ pkgs.bash (import ../pkgs/interception-tools) ] ++ cfg.plugins;
      script = ''
        nice -n -20 udevmon -c \
        ${pkgs.writeText "udevmon.yaml" cfg.udevmonConfig}
      '';
      wantedBy = [ "multi-user.target" ];
    };
  };
}
