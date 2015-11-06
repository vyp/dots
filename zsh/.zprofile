typeset -U path
path=(~/sc/bin ~/ui/bin ~/.guix-profile/bin ~/.cabal/bin $path)
[ ! -s ~/.config/mpd/pid ] && mpd
emacs --daemon &
