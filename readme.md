# Introduction

>> Honestly the real problem is that our first editors are always, notepad,
>> notepad++, gedit, sublime text, word etc. I'd love to see an experiment done
>> on little children wherein their first editor is vim, every input into the
>> computer is via vim (posting a comment on facebook, reddit, typing something
>> out, you name it).

>> I'd like to see how long it would take a kid to be fluent enough to own
>> everyone with his/her vim skills and after learning vim I'd like him to be
>> given the option to use modern editors and see what they choose.

> I'd love to see this experiment only because it would mean that someone
> would have to **make a system where the input fields for every computer
> program are in vim mode. And I want to use that system**.

- [creepingdeathv2; FakingItEveryDay][quote]

[quote]: https://www.reddit.com/r/vim/comments/2ww6fv/this_is_your_brain_on_vim/couym1j

**Disclaimer:** Despite that quote, there is nothing special here. Just trying
to use vim-like keybindings for everything and everywhere possible, and these
quotes serve as further inspiration.

# Contents

The configuration files for each program are under respectively named folders.

Program       | Use
-------       | ---
[Bspwm]       | Window Manager
[dircolors]   | Color Setup for `ls`
[dmenu]       | Launcher
[Dzen]        | Panel
[Emacs]       | Text Editor
[Firefox]     | Web Browser
[Git]         | Version Control
[GTK+]        | Graphical Toolkit
[MPD]         | Music Server
[mpv]         | Video Player
[Mutt]        | Email Client
[Ncmpcpp]     | MPD Client
[Pacman]      | Package Manager
[Pentadactyl] | Web Browser
[rTorrent]    | BitTorrent Client
[SSH]         | Secure Shell
[Sxhkd]       | Keybindings Manager
[Urxvt]       | Terminal Emulator
[Vim]         | Text Editor
[X11]         | Display Server
[Zathura]     | Document (PDF) Viewer
[Zsh]         | Shell

[Bspwm]:       https://github.com/baskerville/bspwm
[dircolors]:   https://www.gnu.org/software/coreutils/manual/html_node/dircolors-invocation.html
[dmenu]:       http://tools.suckless.org/dmenu/
[Dzen]:        http://robm.github.io/dzen/
[Emacs]:       https://www.gnu.org/software/emacs/
[Firefox]:     https://mozilla.org/firefox
[Git]:         http://git-scm.com/
[GTK+]:        http://www.gtk.org/
[MPD]:         http://www.musicpd.org/
[mpv]:         http://mpv.io/
[Mutt]:        http://www.mutt.org/
[Ncmpcpp]:     http://ncmpcpp.rybczak.net/
[Pentadactyl]: http://5digits.org/pentadactyl/
[Pacman]:      https://www.archlinux.org/pacman/
[rTorrent]:    http://rakshasa.github.io/rtorrent/
[SSH]:         http://www.openssh.com/
[Sxhkd]:       https://github.com/baskerville/sxhkd
[Urxvt]:       http://software.schmorp.de/pkg/rxvt-unicode.html
[Vim]:         http://www.vim.org/
[X11]:         http://www.x.org/wiki/
[Zathura]:     https://pwmt.org/projects/zathura/
[Zsh]:         http://www.zsh.org/

# Instructions

With [GNU Stow]:

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

[GNU Stow]: https://www.gnu.org/software/stow/

# Screenshots

*Todo.*

# License

Anything and everything here is [free software] wherever and whenever it
applies: you can redistribute and/or modify them under the terms of the GNU
Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

They are distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. BE CAREFUL and dance with these beasts At YOUR OWN RISK.
See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License within
this repository in a file in the root directory named `license`. If not, see
<http://www.gnu.org/licenses/>.

[free software]: https://www.gnu.org/philosophy/free-sw.html
