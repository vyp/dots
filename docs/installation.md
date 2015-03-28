# Short Version

1. [Install Arch GNU Linux][1].

2.      # pacman -S git stow sudo

3. [Create a 'normal' user][2].

4. [Add such user to sudoers][3]:

        # visudo /etc/sudoers

5. Log in to that account and:

        # __Be sure you are at the home directory.__
        $ cd ~

        # Or git clone from mirror: <https://notabug.org/i/etsi.git>
        $ git clone https://github.com/vyp/etsi 

        $ cd etsi
        $ ./bootstrap

6. (Optional) Download the AUR:

        $ mkdir ~/dl
        $ cd ~/dl
        $ git clone -depth=1 git://pkgbuild.com/aur-mirror.git

7. Install packages (all the programs you want, including from the AUR and
   whatever other sources...).

8. (Assuming `zsh` was installed in step 7:)

        $ chsh -s $(which zsh)

9. Log out and log back in to load zsh (`$ exit` from the tty).

10.     $ cd ~/etsi
        $ antigen restore zsh-plugins-snapshot
        $ ./vim-plugins-snapshot
        $ cd ~

11. Copy over personal files and/or font files.

12.     $ startx

13.     $ firefox

14. Install any/all Firefox addons you want.

15. Step 13 will create a profile directory under `~/.mozilla/firefox/` in the
    form of `########.default` where the `#`'s are (seemingly) random letters
    and/or numbers. Now the Firefox configuration files can be installed:

        $ cd ~/etsi
        $ stow -t ~/.mozilla/firefox/########.default firefox

[1]: https://wiki.archlinux.org/index.php/installation_guide
[2]: https://wiki.archlinux.org/index.php/Users_and_groups#Example_adding_a_user
[3]: https://wiki.archlinux.org/index.php/Sudo#Example_Entries

# Long Version

*Todo.*
