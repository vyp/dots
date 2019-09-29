typeset -U path
path=(~/util/bin ~/code/scripts/bin $path)
export BROWSER=chromium
export EDITOR=vim
export GUILE_LOAD_PATH=~/code/scripts/lib/guile
export READNULLCMD=less
export POLYBAR_WIDTH=$(xrandr | fgrep '*' | cut -dx -f1 | xargs)
export SUDO_EDITOR=rvim
export TERMINAL=termite
export VISUAL=$EDITOR
