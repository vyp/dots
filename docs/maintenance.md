# Updating

1. Packages from official repositories (**be sure to check the [news]
   first!**):

        $ sudo pacman -Syu

2. (Optional) AUR git repository:

        $ cd ~/dl/aur-mirror
        $ git pull

3. AUR packages.

        # List AUR packages.
        $ pacman -Qm

        # Check for updates to AUR packages (assuming [cower] is installed):
        $ cower -u

        # N.B. Git AUR packages (i.e. those AUR packages whose names end in
        # '-git') will obviously not be checked for the latest commit here.
        # Either check manually or try [pacvcs] for that.

4. Global npm packages:

        # Check for updates:
        $ npm-check-updates -g

        # Install any such updates:
        $ sudo npm update -g

5. Hackage packages:

        $ cabal update
        $ cabal install --reinstall world

6. vim-plug:

        $ cd ~/etsi/vim-plug

        # Check for updates:
        $ git fetch --dry-run

        # Update:
        $ git pull

7. Antigen:

        $ cd ~/etsi/antigen

        # Check for updates:
        $ git fetch --dry-run

        # Update:
        $ git pull

8. Vim plugins:

        $ vim +PlugUpdate +only

        # If there are updates, first check if everything still works, but
        # ultimately be sure to update `vim-plugins-snapshot` from vim:

        :PlugSnapshot ~/etsi/vim-plugins-snapshot | only

9. Zsh plugins:

        $ antigen update

        # Similar to the vim plugins, update the `zsh-plugins-snapshot` once
        # confirmed that everything still works:

        $ antigen snapshot ~/etsi/zsh-plugins-snapshot

10. Firefox addons (manually). Although this is fairly optional because of
    Firefox's relatively fast 3 month release cycle. (Each new release will
    automatically require up-to-date addons.)

[news]: https://www.archlinux.org/news/
[cower]: https://github.com/falconindy/cower
[pacvcs]: https://gist.github.com/pzl/123e7fea9a580737b92b
