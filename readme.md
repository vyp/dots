# Philosophy / Goals

1. Everything, or at least amap, is **[free software]**.
2. Everything can be keyboard controlled via **vim-like keybindings**.
    - Comfortable, possibly also reduces risk of [RSI].
    - Less visual clutter if things can be accessed via keybindings without
      needing a visible onscreen icon to click on, meaning less cognitive
      overhead.
    - Efficiency and speed.
3. All configuration is **declarative**\*.
    - **Reproducible**.
    - **Minimal** -- If everything is declarative, there should be nothing
        excess installed or configured.
4. **Free formats** only, preferring **plain text** formats.
    - **Sustainability**:
        - The data does not necessarily die if the current tool or
          implementation does.
        - Simpler formats have a higher chance of being readable and usable many
          years into the future as technology evolves.

Note that this is only for *me*. It may not necessarily work for you, especially
if your workflow is significantly different. For example, if you use the [GNU
Image Manipulation Program][GIMP] a lot, point 2 does not make much sense for
it.

Also note that these are the *goals*, and that this repository may not currently
reflect this philosophy completely. There is still a lot of work to be done to
achieve these goals. However, I am confident it is possible.

\*Obviously except for things like passwords or private keys. (*Even if
encrypted*. Because technology may evolve in unpredictable or unprecedented ways
that nullify current encryption/security algorithms. For example, quantum
computers, in which case your attacker suddenly has a zero day.)

[free software]: https://www.gnu.org/philosophy/free-sw.html
[RSI]:           https://en.wikipedia.org/wiki/Repetitive_strain_injury
[GIMP]:          https://www.gimp.org/

# Contents

The configuration files for each program are under respectively named folders.

Program       | Use
-------       | ---
[Bspwm]       | Window Manager
[dmenu]       | Launcher
[Dzen2]       | Panel
[Emacs]       | Text Editor
[Firefox]     | Web Browser
[Git]         | Version Control
[MPD]         | Music Server
[mpv]         | Video Player
[Ncmpcpp]     | MPD Client
[Pentadactyl] | Web Browser
[Qutebrowser] | Web Browser
[rTorrent]    | BitTorrent Client
[Sxhkd]       | Keybindings Manager
[Urxvt]       | Terminal Emulator
[Vim]         | Text Editor
[xmonad]      | Window Manager
[Zathura]     | Document Viewer
[Zsh]         | Shell

[Bspwm]:       https://github.com/baskerville/bspwm
[dmenu]:       http://tools.suckless.org/dmenu/
[Dzen2]:       https://robm.github.io/dzen/
[Emacs]:       https://www.gnu.org/software/emacs/
[Firefox]:     https://mozilla.org/firefox
[Git]:         http://git-scm.com/
[MPD]:         http://www.musicpd.org/
[mpv]:         http://mpv.io/
[Ncmpcpp]:     http://ncmpcpp.rybczak.net/
[Pentadactyl]: http://5digits.org/pentadactyl/
[Qutebrowser]: https://github.com/The-Compiler/qutebrowser
[rTorrent]:    https://rakshasa.github.io/rtorrent/
[Sxhkd]:       https://github.com/baskerville/sxhkd
[Urxvt]:       http://software.schmorp.de/pkg/rxvt-unicode.html
[Vim]:         http://www.vim.org/
[xmonad]:      http://xmonad.org/
[Zathura]:     https://pwmt.org/projects/zathura/
[Zsh]:         http://www.zsh.org/

# Instructions

With [GNU][1] [Stow][2]:

``` shell
# Install configuration files for a program (e.g. vim).
$ stow vim

# Uninstall.
$ stow -D vim

# Reinstall.
$ stow -R vim
```

These are just basic instructions. Methods/scripts for automatic installation
and maintainence are being worked on (slowly).

[1]: https://gnu.org/
[2]: https://www.gnu.org/software/stow/

# License

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License along
with this program. If not, see <http://www.gnu.org/licenses/>.
