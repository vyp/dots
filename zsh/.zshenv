typeset -U path
fpath=(~/src/comp $fpath)
path=(~/src/bin ~/dev/scripts/bin $path)
export EDITOR=vim
export GUILE_LOAD_PATH=~/dev/scripts/lib/guile
export READNULLCMD=less
export SUDO_EDITOR=rvim
export VISUAL=$EDITOR
