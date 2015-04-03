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

7. Check if there are any extra changes for pacman configuration (and if there
   are, you should probably incoporate them into `./pacman/etc/pacman.conf`,
   because the step 9 will delete `/etc/pacman.conf`):

        $ diff /etc/pacman.conf ~/etsi/pacman/etc/pacman.conf

8. Some ArchHaskell repository keys may need to [added and signed][5] depending
   on pacman configuration. They may also need to be added/signed twice to work
   [for some reason][6]. If there are errors when trying to add such keys, an
   empty `/root/.gnupg/dirmngr_ldapservers.conf` file [may need to be
   created][7]:

        $ sudo mkdir -pv /root/.gnupg
        $ sudo touch /root/.gnupg/dirmngr_ldapservers.conf

9. Preliminary setup:

        $ cd ~/etsi
        $ ./bootstrap

10. (Optional) Download the AUR:

        $ mkdir ~/dl
        $ cd ~/dl
        $ git clone --depth=1 git://pkgbuild.com/aur-mirror.git

11. Install packages (all the programs you want, including from the AUR and
    whatever other sources...).

12. Assuming `zsh` was installed in step 11:

        $ chsh -s $(which zsh)

13. Log out and log back in to load zsh:

        # From the tty:
        $ exit

        # And obviously enter username and password as usual.

14.     $ cd ~/etsi
        $ antigen restore zsh-plugins-snapshot
        $ ./vim-plugins-snapshot
        $ cd ~

15. Copy over personal files and/or font files.

16.     $ startx

17.     $ firefox

18. Install any/all Firefox addons you want.

19. Step 17 will create a profile directory under `~/.mozilla/firefox/` in the
    form of `########.default` where the `#`'s are (seemingly) random letters
    and/or numbers. Now the rest of the Firefox configuration files can be
    installed:

        $ cd ~/etsi

        # Obviously, replace the hashes with whatever the profile directory was
        # named!
        $ stow -t ~/.mozilla/firefox/########.default firefox

[1]: https://wiki.archlinux.org/index.php/installation_guide
[2]: https://aur.archlinux.org/packages/linux-libre/
[3]: https://wiki.archlinux.org/index.php/Users_and_groups#Example_adding_a_user
[4]: https://wiki.archlinux.org/index.php/Sudo#Example_Entries
[5]: https://wiki.archlinux.org/index.php/ArchHaskell#Available_repositories
[6]: https://bbs.archlinux.org/viewtopic.php?id=155889
[7]: https://bbs.archlinux.org/viewtopic.php?id=190380
