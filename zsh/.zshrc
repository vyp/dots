stty start ''
stty stop ''
stty -ixon
stty ixoff
stty ixany

autoload -Uz compinit
compinit
autoload -U colors && colors
autoload edit-command-line
autoload -Uz vcs_info

bindkey -v
bindkey -M vicmd v edit-command-line

setopt autocd extendedglob
setopt autocd
setopt autopushd
# unsetopt beep
setopt no_beep
setopt prompt_subst

zle -N edit-command-line

zstyle ':completion:*' menu select
zstyle :compinstall filename '~/.zshrc'
zstyle ':vcs_info:git*' formats "%b"
zstyle ':vcs_info:*' enable git

# Aliases

alias ,,,,='urxvtc && urxvtc && urxvtc && urxvtc'
alias ,,,='urxvtc && urxvtc && urxvtc'
alias ,,='urxvtc && urxvtc'
alias ,='urxvtc'
alias ,l='ls -p --color=auto'
alias ab='acpi -b'

# By /u/pentothal --
# http://www.reddit.com/r/archlinux/comments/1yivjv/arch_linux_news_linux_313_warning_ps2_keyboard/cfky730
alias an="curl -s https://www.archlinux.org/feeds/news/ | xmllint --xpath //item/title\ \|\ //item/pubDate /dev/stdin | sed -r -e 's:<title>([^<]*?)</title><pubDate>([^<]*?)</pubDate>:\2\t\1\n:g'"

alias bl='echo -e "\a"'
alias c='cat'
alias cb='cp'
alias cn='cat -n'
alias cp='cp -v'
alias cpr='cp -r'

# `nl` apparently more standard than `cat -n`.
alias ds='dirs -p | tail -n +2 | nl'

alias dsv='dirs -v'
alias dt='dirs -c'
alias e='vim'
alias f='file'
alias fd='find'
alias fnd='find . -name'
alias g='git'
alias ga='git add -A'
alias gac='git add -A && git commit'
alias gb='git branch'
alias gc='git commit'
alias gch='git checkout'
alias gchu='git fetch --dry-run'
alias gd='git diff'
alias gi='grep -i'
alias gir='grep -ir'
alias gl='git log'
alias glem='git log --format="%an %ae" | sort | uniq'
alias gm='git merge'
alias gmnoff='git merge --no-ff'
alias gpsh='git push'
alias gpsho='git push origin'
alias gpshom='git push origin master'
alias gpshod='git push origin dev'
alias gpul='git pull'
alias gr='grep'
alias grr='grep -r'
alias gs='git status'
# alias gtypist='gtypist -ist -c 2,0'
alias h='head'
alias info='info --vi-keys'
alias j='jobs'
alias k='cd ../'
alias kk='cd ../../'
alias kkk='cd ../../../'
alias kkkk='cd ../../../../'
alias kl='kill'
alias kl1='kill %1'
alias l='less'
alias l1='ls -1p --color=auto'
alias la='ls -ap --color=auto'
alias la1='ls -a1p --color=auto'
alias lah='ls -lah --color=auto'
alias latr='ls -latr --color=auto'
alias lh='ls -lh --color=auto'
alias ll='less -N'
alias ls='ls -p --color=auto'
alias ltr='ls -trp --color=auto'
alias md='mkdir -pv'
alias mi='mediainfo'
alias mk='make'
# alias mkd='make -f ~/templates/pdf.mk'
alias ml='mpv --loop=inf'
alias mn='mpv --no-video --loop=inf'
alias mv='mv -v'
alias n='cd ~/hak/notera && [[ -n $(pgrep notera) ]] && npm restart || npm start && popd > /dev/null'
alias ncmpcpp='[[ $TERM = "linux" ]] && ncmpcpp -c ~/.config/ncmpcpp/config || urxvtc -name ncmpcpp -e ncmpcpp -c ~/.config/ncmpcpp/config'
alias p8='ping 8.8.8.8'
alias pc='pandoc'
alias pd='popd > /dev/null'
alias pdd='popd > /dev/null && popd > /dev/null'
alias pddd='popd > /dev/null && popd > /dev/null && popd > /dev/null'
alias pdddd='popd > /dev/null && popd > /dev/null && popd > /dev/null && popd > /dev/null'
alias pddddd='popd > /dev/null && popd > /dev/null && popd > /dev/null && popd > /dev/null && popd > /dev/null'
alias pdddddd='popd > /dev/null && popd > /dev/null && popd > /dev/null && popd > /dev/null && popd > /dev/null && popd > /dev/null'

# Install specific package(s) from the repositories.
# (Install)
alias pi='sudo pacman -S'

# Install specific package not from the repositories but from a file.
alias piu='sudo pacman -U'

# Display information about a given package in the local database.
# (information for Local package)
alias pl='pacman -Qi'

# Search for package(s) in the local database.
alias pls='pacman -Qs'

# Display information about a given package in the repositories.
# (information for repository Mirror package)
alias pm='pacman -Si'

# Search for package(s) in the repositories.
alias pms='pacman -Ss'

alias pn='ping -c 3 8.8.8.8'

# Remove the specified package(s), its configuration(s) and unneeded
# dependencies.
# (Remove Configuration)
alias prc='sudo pacman -Rns'

# Remove the specified package(s), retaining its configuration(s) and required
# dependencies.
# (ReMove)
alias prm='sudo pacman -R'

# Synchronize with repositories and then upgrade packages that are out of date
# on the local system.
# (Upgrade)
alias pu='sudo pacman -Syu'

alias q='exit'
# alias rm='rm -i'
alias rm='rm -v'
alias rmdir='rmdir -v'
alias rr='ranger'
alias s='sed'
alias sd='mpc stop; mpd --kill; sudo shutdown now'
alias se='sudoedit'
alias sm='ls ~/music | grep -i'
alias t='tail'
alias v='vim'
alias vc='vim ~/.vimrc'
alias vz='vim ~/.zshrc'
alias z='zathura --fork > /dev/null 2>&1'
alias zz='. ~/.zshrc'
# alias zt='tabbed -cd zathura -e 2>/dev/null'
# alias zv='zathura'

# alias ..='cd ../'
# alias ...='cd ../../'
# alias ....='cd ../../../'
# alias .....='cd ../../../../'
# alias tn='transset-df'
# alias ta='transset-df -a'
# alias tt='transset-df -t'
# alias tat='transset-df -at'

function - {
  if [[ $# -eq 0 ]]; then
    cd "$OLDPWD"
  else
    builtin - "$@"
  fi
}

eval `dircolors ~/.dircolors`
source ~/.zcolors

# 10ms delay (instead of default 400) for key sequences.
KEYTIMEOUT=1

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

# Helper for setting color including all kinds of terminals.
set_prompt_color () {
  if [[ $TERM = "linux" ]]; then
    # Nothing.
  elif [[ $TMUX != '' ]]; then
    printf '\033Ptmux;\033\033]12;%b\007\033\\' "$1"
  else
    echo -ne "\033]12;$1\007"
  fi
}

# Change cursor color basing on vi mode.
zle-keymap-select () {
  if [ $KEYMAP = vicmd ]; then
    set_prompt_color $COMMAND_PROMPT
  else
    set_prompt_color $INSERT_PROMPT
  fi
}

zle-line-finish () {
  set_prompt_color $INSERT_PROMPT
}

zle-line-init () {
  zle -K viins
  set_prompt_color $INSERT_PROMPT
}

zle -N zle-keymap-select
zle -N zle-line-init
zle -N zle-line-finish

precmd() {
  vcs_info
}

# Other possible prompt characters: > » ≻ ➤
PROMPT="%{$reset_color%} ${CHAR}»%{$reset_color%} "
RPROMPT='%{$reset_color%}${BGJOBS}%(1j.%j.) %{$reset_color%}${VCS_INFO}${vcs_info_msg_0_} %{$reset_color%}${DIR}%~%{$reset_color%} '

source ~/etsi/antigen/antigen.zsh

antigen bundle zsh-users/zsh-syntax-highlighting
antigen apply

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)

# Disable underlines.
ZSH_HIGHLIGHT_STYLES[precommand]=fg=$ZSH_HIGHLIGHT_STYLES_PRECOMMAND
ZSH_HIGHLIGHT_STYLES[path]=none
ZSH_HIGHLIGHT_STYLES[path_prefix]=none
ZSH_HIGHLIGHT_STYLES[path_approx]=fg=$ZSH_HIGHLIGHT_STYLES_PATH_APPROX

# # SP_VIDPLAYER=cvlc
# # SP_VIDPLAYER=vlc
# # SP_VIDPLAYER=(vlc --control=lirc)
# # SP_VIDPLAYER=mplayer
# # SP_VIDPLAYER=(mplayer -cache 8192)
# SP_VIDPLAYER=(mplayer -cache 131072)
# # SP_VIDPLAYER=(mpv --cache=8192)
# 
# # Wait X seconds to stabilize channel (make it longer if you have a slower
# # connection).
# SP_SLEEP=30
# 
# # Sopcast port and player port.
# SP_LOCAL_PORT=55050
# SP_PLAYER_PORT=55051
# 
# # Manually kill sopcast (sometimes it doesnt exit properly and still uses
# # bandwidth in the background).
# sppc-kill() { killall sp-sc ;}
# 
# # Kills existing connection, starts a new connection, sleep X sec to
# # stabilize the stream, waits to player to exit and kill itself.
# sppc() {
#   killall sp-sc &>/dev/null
#   (sp-sc "$1" $SP_LOCAL_PORT $SP_PLAYER_PORT &>/dev/null &)
#   sleep $SP_SLEEP
#   ($SP_VIDPLAYER http://localhost:$SP_PLAYER_PORT)
#   wait
#   killall sp-sc
# }

#### eng = english, ro = romanian, esp = espanol/spanish
# added on February 06, 2014
# spp-doc-explorer.eng,ro() { sppc "sop://broker.sopcast.com:3912/149269" ;}
# spp-doc-history.eng,ro() { sppc "sop://broker.sopcast.com:3912/148253" ;}
# spp-doc-history2.eng,ro() { sppc "sop://broker.sopcast.com:3912/149268" ;}
# spp-doc-natgeo.eng,ro() { sppc "sop://broker.sopcast.com:3912/148248" ;}
# spp-doc-natgeowild.eng,ro() { sppc "sop://broker.sopcast.com:3912/148259" ;}
# spp-doc-nature.eng,ro() { sppc "sop://broker.sopcast.com:3912/149267" ;}
# spp-movie-hbo.eng,ro() { sppc "sop://broker.sopcast.com:3912/148883" ;}
# spp-movie-hbo2.eng,ro() { sppc "sop://broker.sopcast.com:3912/120702" ;}
# spp-tv-universal.eng,ro() { sppc "sop://broker.sopcast.com:3912/148255" ;}
# spp-tv-axn.eng,ro() { sppc "sop://broker.sopcast.com:3912/148257" ;}
# spp-tv-axncrime.eng,ro() { sppc "sop://broker.sopcast.com:3912/149261" ;}
