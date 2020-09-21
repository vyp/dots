{
  description = "system config";

  inputs.nixpkgs.url = github:nixos/nixpkgs-channels/nixos-unstable;

  outputs = { nixpkgs, nix, self, ... }@inputs: {
    nixosConfigurations = with nixpkgs.lib;
      let
        flakeInputs = inputs;
        hosts = map (fname: builtins.head (builtins.match "(.*)\\.nix" fname))
          (builtins.attrNames (builtins.readDir ./nixos/devices));
        mkHost = deviceName:
          nixosSystem {
            system = "x86_64-linux";
            modules = [ (import ./nixos/config.nix) ];
            specialArgs = { inherit flakeInputs deviceName; };
          };
      in genAttrs hosts mkHost;
  };
}
