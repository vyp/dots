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

   Many many thanks to **Dr. Richard Stallman** for creating the entire [free
   software][free-sw] ideology and culture, including **[GNU]**. I can imagine
   the world would be a *much worse* place if he did not. 🙇‍♀️
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

# Installation

0. <details><summary><strong>TL;DR</strong></summary>

   1.  Download latest nixos-unstable image from
       https://nixos.org/channels/nixos-unstable

   2.  Make a bootable usb:

       ``` shell
       dd if=path/to/image of=/dev/sdb
       ```

   3.  Boot into live image and log in as root with empty password.

   4.  Get internet access.

   5.  Partition and format disks.

   6.  Mount target filesystems under `/mnt`:

       ``` shell
       mount /dev/disk/by-label/nixos /mnt
       ```

   7.  *(optional)* Activate swap device: `swapon <device>`.

   8.  Copy `wpa_supplicant.conf` to target filesystem:

       ``` shell
       cp /etc/wpa_supplicant.conf /mnt/etc
       ```

       This allows wpa_supplicant to automatically connect to internet when
       rebooting into the installed system.

   9.  Generate `/etc/hardware-configuration.nix`:

       ``` shell
       nixos-generate-config --root /mnt
       ```

   10. Backup `/etc/hardware-configuration.nix`:

       ``` shell
       cp /mnt/etc/nixos/hardware-configuration.nix \
          /mnt/etc/nixos/hardware-configuration.modified.nix
       ```

       and put `system.stateVersion` from `/mnt/etc/nixos/configuration.nix`
       into `hardware-configuration.modified.nix` and edit the latter further if
       necessary.

       Also don't forget to set the bootloader in
       `hardware-configuration.modified.nix`, see `nixos/config.nix` for
       examples near the beginning of the file. Inspecting the generated
       `configuration.nix` to see if one of the options was already put there
       can also give a hint on which one to choose.

   11. Run `nixos-install` with `minimal.nix`:

       ``` shell
       cd /mnt/etc/nixos
       nixos-install \
       -I nixos-config=https://github.com/vyp/dots/raw/master/nixos/minimal.nix
       ```

   12. Reboot and login with root and set password for user "u":

       ``` shell
       passwd u
       ```

   13. Login as user and retrieve this repository:

       ``` shell
       nix-env -i git
       git clone --recursive https://vyp@github.com/vyp/dots
       ```

       This may take a little while as the nixpkgs repository is a submodule and
       at the time of writing it's about 500MB in size.

   14. Run the bootstrap script which essentially stows all the dotfiles:

       ``` shell
       ./dots/bootstrap
       ```

   15. `sudo nixos-rebuild boot` and reboot (`sudo shutdown now`).
   </details>

1. <details><summary><strong>Bootable Image</strong></summary>

   </details>

2. <details><summary><strong>Internet Access</strong></summary>

   1. `ip a` will bring up a list of network interfaces.

   2. `iwlist <interface> scan | less` to see if your wifi is available.

   3. Edit `/etc/wpa_supplicant.conf` with your network details.

   4. `wpa_supplicant -B -i<interface> -c/etc/wpa_supplicant.conf -Dwext`.

   5. `dhclient <interface>` or `dhcpcd <interface>` if `dhclient` command
      doesn't exist.
   </details>

3. <details><summary><strong>Hardware Setup</strong></summary>

   1. `lsblk -f` lists your devices.

   2. `mkfs.ext4 -L nixos <device>` to format a device.

   3. Similarly, `mkswap -L swap <device>` to make a swap partition.
   </details>

4. <details><summary><strong>Magic ✨</strong></summary>

   </details>

5. <details><summary><strong>Initialization</strong></summary>

   1. Add some remotes to local nixpkgs repository:

      ``` shell
      cd ~/dots/nixos/nixpkgs
      git remote add channels https://github.com/nixos/nixpkgs-channels
      git remote add fork https://vyp@github.com/vyp/nixpkgs
      ```

      The reason nixpkgs is used as the submodule and not nixpkgs-channels
      directly is that the former allows cherry picking commits from latest
      master to get any potentially new package definitions not available in
      unstable. So it is a bit more flexible I suppose.
   </details>

# 📢 Shoutouts 📢

‼ I've probably missed some people, which I apologize for in advance.
Unfortunately, it would be close to impossible to list *everyone* who deserves
credit anyway, because we're all really standing on the shoulders of enormous
giants in the first place.

- Bill Joy for creating [vi] and it's unique but extremely effective model of
  editing, which inspires many aspects of this setup.
- [@brammool] for creating [vim], which is my fallback editor. It's like a
  'modern' more usable version of vi.
- [@civodul] for creating [guix].
- [Dr. Richard Stallman][rms] for creating [emacs] and [GNU].
- Eben Moglen and [@bkuhn] for being copyleft champions.
- [@edolstra] for creating [NixOS].
- [@epsil] for creating [evil mode].
- [@t-wissmann] for creating [herbstluftwm].
  - Also helped me with creating a solution to move to the last focused window:
    https://github.com/herbstluftwm/herbstluftwm/issues/98#issuecomment-316342968
- Tom Lord for creating [guile].
  - And [@jimblandy], [@othermaciej], Mikael Djurfeldt, [@mvollmer],
    [@neiljerram], [@civodul] and [@wingo] for maintaining it over the years.
- [@ypnos] for helping me with creating a better solution to move to the last
  focused window:
  https://github.com/herbstluftwm/herbstluftwm/issues/98#issuecomment-316330616

[@bkuhn]:       https://github.com/bkuhn
[@brammool]:    https://github.com/brammool
[@civodul]:     https://github.com/civodul
[@edolstra]:    https://github.com/edolstra
[@epsil]:       https://github.com/epsil
[@jimblandy]:   https://github.com/jimblandy
[@mvollmer]:    https://github.com/mvollmer
[@neiljerram]:  https://github.com/neiljerram
[@othermaciej]: https://github.com/othermaciej
[@t-wissmann]:  https://github.com/t-wissmann
[@wingo]:       https://github.com/wingo
[@ypnos]:       https://github.com/ypnos
[emacs]:        https://www.gnu.org/software/emacs
[evil mode]:    https://github.com/emacs-evil/evil
[guile]:        https://www.gnu.org/software/guile/
[guix]:         https://www.gnu.org/software/guix/
[herbstluftwm]: http://www.herbstluftwm.org
[rms]:          https://stallman.org/
[vi]:           https://en.wikipedia.org/wiki/Vi
[vim]:          http://www.vim.org/
