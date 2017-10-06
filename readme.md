<!-- vim: syntax=off
-->
# Screenshots 📸

## Most Recent Preview 🖼

Empty for now.

## Archive

<details>
  <summary>Click me to open. 👈</summary><br>

  Also empty. 👀
</details>

# #goals

1. **Free Software:**

   <details>
   <summary>I should be able to use, study, modify, and share my modifications
   of all the software used in the setup. 🙈🙉🙊</summary><br>

   Many many thanks to [**Dr. Richard Stallman**][rms] for creating the entire
   [free software][free-sw] ideology and culture, including **[GNU]**. I can
   imagine the world would be a *much worse* place if he did not. 🙌
   </details>

2. **Reproducibility:**

   <details>
   <summary>I should be able to easily but flexibly use exactly the same setup
   across my various machines.</summary><br>

   **[NixOS]** naturally plays an extremely integral part here because of it's
   clean, functional and declarative approach to package and configuration
   management.

   I think that the world would be a much better place if more people used Nix!
   So please check it out! ✨
   </details>

3. **Comfort and Safety:**

   <details>
   <summary>I should be physically comfortable and safe when using my setup.
   💆</summary><br>
   </details>

[free-sw]: https://www.gnu.org/philosophy/free-sw.en.html
[GNU]:     https://www.gnu.org/gnu/gnu.en.html
[NixOS]:   https://nixos.org
[rms]:     https://stallman.org/

# Installation

<details><summary>👢</summary>

1.  Download latest nixos-unstable image from
    <https://nixos.org/channels/nixos-unstable>.

2.  Make a bootable usb:

    ``` shell
    # As root.
    dd if=path/to/image of=/dev/sdb
    ```

3.  Boot into live image and log in as root with empty password (if it doesn't
    automatically log you in).

4.  Get internet access.

    1. `ip a` will bring up a list of network interfaces.

    2. `iwlist <interface> scan | less` to see if your wifi is available.

    3. Edit `/etc/wpa_supplicant.conf` with your network details.

    4. `wpa_supplicant -B -i<interface> -c/etc/wpa_supplicant.conf -Dwext`.

    5. `dhclient <interface>` or `dhcpcd <interface>` if `dhclient` command
       doesn't exist.

5.  Partition and format disks.

    - `lsblk -f` lists your devices.

    - `mkfs.ext4 -L nixos <device>` to format a device.

    - `mkswap -L swap <device>` to make a swap partition.

6.  *(optional)* Activate swap device: `swapon <device>`.

7.  Mount target filesystems under `/mnt`:

    ``` shell
    mount /dev/disk/by-label/nixos /mnt
    ```

8.  Generate `/etc/nixos` configuration files:

    ``` shell
    nixos-generate-config --root /mnt
    ```

9.  Inspect the generated `/mnt/etc/configuration.nix` to see if any bootloader
    options were put in there to hint on which bootloader options to use.

10. If using systemd-boot, mount the boot partition under `/mnt/boot` and
    perform step 8 again to get an updated `hardware-configuration.nix` with the
    `/mnt/boot` filesystem entry.

11. Backup `hardware-configuration.nix`:

    ``` shell
    cp /mnt/etc/nixos/hardware-configuration.nix \
       /mnt/etc/nixos/hardware-configuration.modified.nix
    ```

12. Edit `hardware-configuration.modified.nix` to put the correct bootloader
    options in it, and also put `system.stateVersion` from `configuration.nix`
    into `hardware-configuration.modified.nix`.

13. Copy `wpa_supplicant.conf` to target filesystem:

    ``` shell
    cp /etc/wpa_supplicant.conf /mnt/etc
    ```

    This allows wpa_supplicant to automatically connect to internet when
    rebooting into the installed system.

14. `nixos-install -I
    nixos-config=https://raw.githubusercontent.com/vyp/dots/master/nixos/minimal.nix`.

15. Reboot and login with root and set password for user "u":

    ``` shell
    passwd u
    ```

16. Login as user and retrieve this repository:

    ``` shell
    nix-env -i git
    git clone --recursive https://vyp@github.com/vyp/dots
    ```

    This may take a little while as the nixpkgs repository is a submodule and at
    the time of writing it's about 590MB in size.

17. `./dots/bootstrap`.

18. `sudo nixos-rebuild boot` and reboot (`sudo shutdown now`).
</details>
