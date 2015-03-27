# Short Version

1. [Install Arch GNU Linux][1].

2.      # pacman -S git stow

3. [Create a 'normal' user][2] and log in to it.

4.      # __Be sure you are at the home directory.__
        $ cd ~

        # Or from mirror: <https://notabug.org/i/etsi.git>
        $ git clone https://github.com/vyp/etsi 

        $ cd etsi
        $ ./bootstrap

5. (Optional) Download the AUR:

        $ mkdir ~/dl
        $ cd ~/dl
        $ git clone -depth=1 git://pkgbuild.com/aur-mirror.git

6. Install packages (all the programs you want, including from the AUR and
   whatever other sources...).

7.      # Assuming `zsh` was installed in step 6:
        $ chsh -s $(which zsh)


8. Log out and log back in to load zsh (`$ exit` from the tty).

9.      $ cd ~/etsi
        $ antigen restore zsh-plugins-snapshot
        $ ./vim-plugins-snapshot
        $ cd ~

10. Copy over personal files and/or font files.

11.     $ startx

12.     $ firefox

13. Install any/all Firefox addons you want.

14. Step 12 will create a profile directory under `~/.mozilla/firefox/` in the
    form of `########.default` where the `#`'s are (seemingly) random letters
    and/or numbers. Now the Firefox configuration files can be installed:

        $ cd ~/etsi
        $ stow -t ~/.mozilla/firefox/########.default firefox

[1]: https://wiki.archlinux.org/index.php/installation_guide
[2]: https://wiki.archlinux.org/index.php/Users_and_groups#Example_adding_a_user

# Long Version

*Todo.*
