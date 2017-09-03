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

4. **Rice:**

   <details>
   <summary>I should be viewing a 'modern' looking system.</summary><br>
   </details>

5. **Minimalism:**

   <details>
   <summary>I should not have to waste time or energy with superfluous
   stuff.</summary><br>
   </details>

[free-sw]: https://www.gnu.org/philosophy/free-sw.en.html
[GNU]:     https://www.gnu.org/gnu/gnu.en.html
[NixOS]:   https://nixos.org

# Installation

<details>
<summary>**TL;DR**</summary>

1.  Download latest nixos-unstable image from
    https://nixos.org/channels/nixos-unstable

2.  Make a bootable usb:

    ``` shell
    dd if=path/to/image of=/dev/sdb
    ```

3.  Boot into live image and log in as root with empty password.

4.  Get internet access.

5.  Partition and format disks.

6.  Mount target filesystems under `/mnt`.

7.  *(optional)* Activate swap device: `swapon <device>`.

8.  Generate `/etc/hardware-configuration.nix`:

    ``` shell
    nixos-generate-config --root /mnt
    ```

9.  Backup `/etc/hardware-configuration.nix`:

    ``` shell
    cp /mnt/etc/nixos/hardware-configuration.nix \
       /mnt/etc/nixos/hardware-configuration.modified.nix
    ```

    and edit `hardware-configuration.modified.nix` if necessary.

10. Retrieve this repository:

    ``` shell
    nix-env -i git stow
    mkdir -pv /mnt/home/u
    cd /mnt/home/u
    git clone --recursive https://github.com/vyp/dots
    ```

    This may take a little while as the nixpkgs repository is a submodule and at
    the time of writing it's about 500MB in size.

11. Add nixpkgs-channels as a remote:

    ``` shell
    cd dots/nixos/nixpkgs
    git remote add channels https://github.com/nixos/nixpkgs-channels
    ```

    The reason nixpkgs is used as a submodule and not nixpkgs-channels directly
    is that the former allows cherry picking commits from latest master to get
    any potentially new package definitions not available in unstable. So it is
    a bit more flexible I suppose.

12. Run the init script which essentially stows all the dotfiles (doesn't exist
    yet).

13. Initiate main installation command:

    ``` shell
    nixos-install -I nixos-config=/mnt/home/u/dots/nixos/config.nix \
                  -I nixpkgs=/mnt/home/u/dots/nixos/nixpkgs
    ```

    This may actually fail because `config.nix` imports from absolute path
    `/etc/hardware-configuration.modified.nix` instead of a relative path.
    However, --root /mnt is implicit to nixos-install (if unspecified), so maybe
    it will not. Have to try it out. If it does fail, the solution would be to
    simply use a relative path, which would work regardless.

14. Reboot, but before you can login you need to set a password for your user.
    Press ctrl+alt+f1 at the login screen to switch to a virtual tty, login as
    root (the previous step will have prompted you to set a root password), and
    run `passwd u` to set a password for user "u".

15. Logout with ctrl+d and login as your user in the virtual tty still, and run
    `fc-cache -fv` to setup fonts. Logout with ctrl+d again and switch back to X
    with ctrl+alt+f7 and login normally! ☺️
</details>

## Bootable Image

## Internet Access

## Hardware Setup

## Magic ✨

## Initialization

# 📢 Shoutouts 📢

‼ I've probably missed some people, which I apologize for in advance.
Unfortunately, it would be close to impossible to list *everyone* who deserves
credit anyway.

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
