1. [Install Arch GNU][1].

2. Switch to [linux-libre][2]. If this is not possible, place your hardware in
   a fire and get new hardware that can run linux-libre **before continuing any
   further**.

3.      # pacman -S git stow sudo

4. [Create a 'normal' user][3].

5. [Add such user to sudoers][4]:

        # visudo /etc/sudoers

6. Log in to that account and:

        # __Be sure you are at the home directory.__
        $ cd ~
        $ git clone https://github.com/vyp/ui

7. Check if there are any extra changes for pacman configuration (and if there
   are, you should probably incoporate them into `./pacman/etc/pacman.conf`,
   because **step 8 will delete `/etc/pacman.conf`**):

        $ diff /etc/pacman.conf ~/ui/pacman/etc/pacman.conf

8. Preliminary setup:

        $ cd ~/ui
        $ ./bootstrap

9. Install packages (all the programs you want, including from the AUR and
   whatever other sources...).

10. Assuming `zsh` was installed in step 9:

        $ chsh -s $(which zsh)

11. Log out and log back in to load zsh:

        # From the tty:
        $ exit

        # And obviously enter username and password as usual.

12.     $ cd ~/ui
        $ antigen restore zsh-plugins-snapshot
        $ ./vim-plugins-snapshot
        $ cd ~

13. Copy over personal files and/or font files.

14.     $ startx

15.     $ firefox

16. Install any/all Firefox addons you want.

17. Step 15 will create a profile directory under `~/.mozilla/firefox/` in the
    form of `########.default` where the `#`'s are (seemingly) random letters
    and/or numbers. Now the rest of the Firefox configuration files can be
    installed:

        $ cd ~/ui

        # Obviously, replace the hashes with whatever the profile directory was
        # named!
        $ stow -t ~/.mozilla/firefox/########.default firefox

[1]: https://wiki.archlinux.org/index.php/installation_guide
[2]: https://aur.archlinux.org/packages/linux-libre/
[3]: https://wiki.archlinux.org/index.php/Users_and_groups#Example_adding_a_user
[4]: https://wiki.archlinux.org/index.php/Sudo#Example_Entries
