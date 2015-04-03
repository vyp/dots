# Updating

1. Packages from official repositories (**be sure to check the [news]
   first!**):

        $ sudo pacman -Syu

2. (Optional) AUR git repository:

        $ cd ~/dl/aur-mirror
        $ git pull

3. AUR packages. (List them with `$ pacman -Qm`.)

4. Global npm packages:

        $ sudo npm update -g

5. vim-plug:

        $ cd ~/etsi/vim-plug
        $ git pull

6. Antigen:

        $ cd ~/etsi/antigen
        $ git pull

5. Vim plugins:

        $ vim +PlugUpdate +only

        # If there are updates, first check if everything still works, but
        # ultimately be sure to update `vim-plugins-snapshot` from vim:

        :PlugSnapshot ~/etsi/vim-plugins-snapshot | only

6. Zsh plugins:

        $ antigen update

        # Similar to the vim plugins, update the `zsh-plugins-snapshot` once
        # confirmed that everything still works:

        $ antigen snapshot ~/etsi/zsh-plugins-snapshot

7. Firefox addons (manually). Although this is fairly optional because of
   Firefox's relatively fast 3 month release cycle. (Each new release will
   automatically require up-to-date addons.)

[news]: https://www.archlinux.org/news/
