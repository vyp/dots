{
  description = "system config";

  inputs.nixpkgs.url = github:nixos/nixpkgs-channels/nixos-unstable;

  outputs = { nixpkgs, nix, self, ... }@inputs: {
    nixosConfigurations = with nixpkgs.lib;
      let
        hosts = map (fname: builtins.head (builtins.match "(.*)\\.nix" fname))
          (builtins.attrNames (builtins.readDir ./nixos/devices));
        mkHost = name:
          nixosSystem {
            system = "x86_64-linux";
            modules = [ (import ./nixos/config.nix) ];
            specialArgs = { inherit inputs name; };
          };
      in genAttrs hosts mkHost;
  };
}
