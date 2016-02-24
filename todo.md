# Documentation

- installation.
    - installed packages.
- maintainence.
    - updating.
    - files/directory structure.

# Emacs

- Behaviour.
    - Indent by 2 spaces instead of 4 for shell-script-mode.
    - Fix tab keybinding for evil-insert-state.
    - Check out https://github.com/mrkkrp/vimish-fold.
    - Expand region.
    - Emmet.
    - Flycheck.
    - Deft.
    - Paredit.
    - Smartparens and evil-smartparens.
    - Multiple cursors.
    - Highlight trailing newlines.
    - Highlight TODOs.
    - Transposing (including words).
    - Javascript mode 2 space indentation instead of 4 space.
    - wellle/targets.vim ?

- Keybindings.
    - Allow `gg` in ibuffer by removing `g` for refresh.
    - "Completion List", "Packages", "Compile-log", "Messages", "Backtrace"
      buffers.

- Appearance.
    - indicate-buffer-boundaries.
    - Report company mode popup bug on right window edge?
    - Disable fill column indicator in magit mode buffers?
    - Remove right fringe in pdf-view-mode.
    - Statusbar colors and customisation.
        - Smart mode line?

- Special.
    - Magit.
    - Filesystem navigation, opening files, managing buffers etc.
    - Ido.
    - Helm.
    - Terminal/eshell/zsh.
    - Pandoc mode.
    - MPD client.
    - Image viewer.
    - IRC.
    - Mail.
    - RSS.
    - Bittorrent client.
    - Figure out installation method for pdf-tools (whether to use guix, or to
      use quelpa...).

# Themes

## Gruvbox-light

- qutebrowser
    - move qutebrowser theme to themes directory => Makefile.
        - this can probably only happen after
          <https://github.com/The-Compiler/qutebrowser/issues/499> or
          <https://github.com/The-Compiler/qutebrowser/issues/381> because the
          [colors] section will be re-added after every update anyway.
    - remove coloured backgrounds from different status bar types, and make them
      coloured foregrounds instead.
- pentadactyl.

## Redbelly

- probably everything except emacs.

# Vim

- simplify 'centeronsearch' part?
- junegunn/vim-easy-align ?

# Other

- fix dmenurc nonsense.
- git grep for inappropriate shebangs and replace them with more appropriate
  ones.
- use environment variables for theme settings, as much as possible.
    - btw, xmonad and the panel's settings have gruvbox-light theme settings
      hardcoded into them, which obviously needs to be fixed, most likely by
      this.
- remove fonts directory because i don't really care about being able to switch
  fonts as easily as themes, because usage has determined that they are much
  more static choices.
    - actually the different sizes at least are useful because different
      machines/displays/resolutions require different settings.
- copy over the patch and generic.nix files for custom rustc nix expressions?
- qutebrowser git clone in bootstrap script should somehow correspond/checkout
  to the correct commit.
- add limey to fonts options => figure out size issue (because it's bitmap
  anyway), and find out if it required negative letter space..
- implement failsafe to open plain xterm if default xinitrc fails to load.
- make the vim swap file section in the global gitignore (and gitignores of
  other repositories) the same as the one in this repository.
- shell => search command history.
- rewrite upd script to only update emacs packages if :files (from recipe)
  changed.
- use relative paths in scripts by first cd'ing to the directory the script is
  located in: <http://stackoverflow.com/a/246128/4093264> ?
    - OR, use environment variables instead ??
- pandoc and latex templates?
