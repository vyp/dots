typeset -U path
path=(~/sc/bin ~/ui/bin ~/.guix-profile/bin ~/.cabal/bin $path)
emacs --daemon
[ ! -s ~/.config/mpd/pid ] && mpd
