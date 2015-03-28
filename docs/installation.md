# Short Version

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

        # Or git clone from mirror: <https://notabug.org/i/etsi.git>
        $ git clone https://github.com/vyp/etsi 

        $ cd etsi
        $ ./bootstrap

7. (Optional) Download the AUR:

        $ mkdir ~/dl
        $ cd ~/dl
        $ git clone --depth=1 git://pkgbuild.com/aur-mirror.git

8. Install packages (all the programs you want, including from the AUR and
   whatever other sources...).

9. (Assuming `zsh` was installed in step 8:)

        $ chsh -s $(which zsh)

10. Log out and log back in to load zsh (`$ exit` from the tty).

11.     $ cd ~/etsi
        $ antigen restore zsh-plugins-snapshot
        $ ./vim-plugins-snapshot
        $ cd ~

12. Copy over personal files and/or font files.

13.     $ startx

14.     $ firefox

15. Install any/all Firefox addons you want.

16. Step 14 will create a profile directory under `~/.mozilla/firefox/` in the
    form of `########.default` where the `#`'s are (seemingly) random letters
    and/or numbers. Now the Firefox configuration files can be installed:

        $ cd ~/etsi

        # Obviously replace the hashes with whatever the profile directory was
        # named!
        $ stow -t ~/.mozilla/firefox/########.default firefox

[1]: https://wiki.archlinux.org/index.php/installation_guide
[2]: https://aur.archlinux.org/packages/linux-libre/
[3]: https://wiki.archlinux.org/index.php/Users_and_groups#Example_adding_a_user
[4]: https://wiki.archlinux.org/index.php/Sudo#Example_Entries

# Long Version

*Todo.*
