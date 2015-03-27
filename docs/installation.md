# Short Version

1. [Install Arch GNU Linux][1].

2. `# pacman -S git stow`

3. [Create a 'normal' user][2] and log in to it.

4. `$ cd ~` (Be sure you are at the home directory.)

5. `$ git clone https://github.com/vyp/etsi` (Or mirror:
   <https://notabug.org/i/etsi>.)

6. `$ cd etsi`

7. `./bootstrap`

8. (Optional) Download the AUR:

        $ mkdir ~/dl
        $ cd ~/dl
        $ git clone -depth=1 git://pkgbuild.com/aur-mirror.git

9. Install packages (all the programs you want, including from the AUR and
   whatever other sources...).

10. `$ chsh -s $(which zsh)` (Assuming `zsh` was installed in step 9.)

11. Log out and log back in to load zsh (`$ exit` from the tty).

12. Copy over personal files and/or font files.

13. `$ startx`

14. `$ firefox`

15. Install any/all Firefox extensions you want.

16. Step 14 will create a profile directory under `~/.mozilla/firefox/` in the
    form of `########.default` where the `#`'s are (seemingly) random letters
    and/or numbers. Now the Firefox configuration files can be installed:

        $ cd ~/etsi
        $ stow -t ~/.mozilla/firefox/########.default firefox

[1]: https://wiki.archlinux.org/index.php/installation_guide
[2]: https://wiki.archlinux.org/index.php/Users_and_groups#Example_adding_a_user

# Long Version

*Todo.*
