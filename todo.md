# Documentation

- installation.
    - installed packages.
- maintainence.
    - updating.
    - files/directory structure.
- fonts.

# Emacs

- Behaviour.
    - There is a little unexpected behaviour after an update. If a
      'dependency-only' package is updated, it will *not* be rebuilt unless the
      package that depends on it is also updated. Obviously, this may cause
      breakages, but the package.el library in emacs 25 may already solve this..
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

- pentadactyl.

## Redbelly

- probably everything except emacs.

# Vim

- simplify 'centeronsearch' part?
- junegunn/vim-easy-align ?

# Other

- use relative paths in scripts by first cd'ing to the directory the script is
  located in: <http://stackoverflow.com/a/246128/4093264> ?
- log evil updates ?
- pandoc and latex templates?
