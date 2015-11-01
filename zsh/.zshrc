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

cdpath=(~)

bindkey -v
bindkey -M vicmd v edit-command-line

setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_ignore_space
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
alias cr='crystal'

# `nl` apparently more standard than `cat -n`.
alias ds='dirs -p | tail -n +2 | nl'

alias dsv='dirs -v'
alias dt='dirs -c'
alias e='emacsclient -c'
alias er='emacsclient -e "(kill-emacs)" && emacs --daemon'
alias et='emacsclient'
alias f='file'
alias fd='find'
alias fnd='find . -name'
alias g='git'
alias ga='git add'
alias gac='git add -A && git commit'
alias gall='git add -A'
alias gap='git add -p'
alias gb='git branch'
alias gc='git commit'
alias gcem='git commit --edit -m "$(git log --format=%B --reverse HEAD..HEAD@{1})"'
alias gch='git checkout'
alias gchu='git fetch --dry-run'
alias gd='git diff'
alias gds='git diff --staged'
alias ge='git status --short | while read first rest; do if [[ $first == *"M"* ]] || [[ $first == *"A"* ]] || [[ $first == *"?"* ]]; then echo $rest; fi; done | awk '"'"'{print $NF}'"'"' | while read gspath; do [[ ${gspath: -1} == "/" ]] || echo $gspath; done | while read filepath; do [[ -e $filepath ]] && echo $filepath; done | while read existingfile; do [[ "$(file -bL $existingfile)" == *"text"* ]] && echo $existingfile; done | xargs bash -c '"'"'</dev/tty $EDITOR "$@"'"'"' i'
alias gi='grep -i'
alias gir='grep -ir'
alias gl='git log'
alias glem='git log --format="%an %ae" | sort | uniq'
alias gm='git merge'
alias gmnoff='git merge --no-ff'
alias gpsh='git push'
alias gpul='git pull'
alias gr='grep'
alias grr='grep -r'
alias grs='git reset'
alias gs='git status'
alias gsh='git show'
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
alias la='ls -Ap --color=auto'
alias la1='ls -A1p --color=auto'
alias lah='ls -lAh --color=auto'
alias latr='ls -lAtr --color=auto'
alias lh='ls -lh --color=auto'
alias ll='less -N'
alias ls='ls -p --color=auto'
alias ltr='ls -trp --color=auto'
alias md='mkdir -pv'
alias mi='mediainfo'
alias mk='make'
alias ml='mpv --loop=inf'
alias mn='mpv --no-video --loop=inf'
alias mutt='mutt -F ~/.config/mutt/muttrc'
alias mv='mv -v'
alias ncmpcpp='ncmpcpp -c ~/.config/ncmpcpp/config'
alias npm='npm --no-color'
alias p8='ping 8.8.8.8'
alias pc='pandoc'
alias pd='popd > /dev/null'
alias pdd='popd > /dev/null && popd > /dev/null'
alias pddd='popd > /dev/null && popd > /dev/null && popd > /dev/null'
alias pfd='popd > /dev/null && popd > /dev/null && popd > /dev/null && popd > /dev/null'
alias pffd='popd > /dev/null && popd > /dev/null && popd > /dev/null && popd > /dev/null && popd > /dev/null'
alias psd='popd > /dev/null && popd > /dev/null && popd > /dev/null && popd > /dev/null && popd > /dev/null && popd > /dev/null'

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

alias pt='ping -c 3 8.8.8.8; while [[ $? -eq 1 ]]; do ping -c 3 8.8.8.8 && echo -e "\a"; done'

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

# TODO: Use git grep instead if inside git repository directory.
# Probably also use tput to highlight the instances of "TODO" in some way and
# change this into a small shell script.
alias todo='grep -rI -A 2 --exclude-dir={.git,antigen,bower_components,node_modules,vim-plug} --exclude="colors.penta" TODO:'

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
HISTSIZE=1000000
SAVEHIST=1000000

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

# Other possible prompt characters: > » ≻ ➤ ─ ━
PROMPT="%{$reset_color%} %(?.${CHAR}.${ERROR})━━━%{$reset_color%}%b "
RPROMPT='%{$reset_color%}${BGJOBS}%(1j.%j.) %{$reset_color%}${VCS_INFO}${vcs_info_msg_0_} %{$reset_color%}${DIR}%~%{$reset_color%} '

source ~/ui/vendor/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)

# Disable underlines for zsh-syntax-highlighting.
ZSH_HIGHLIGHT_STYLES[precommand]=fg=$ZSH_HIGHLIGHT_STYLES_PRECOMMAND
ZSH_HIGHLIGHT_STYLES[path]=none
ZSH_HIGHLIGHT_STYLES[path_prefix]=none
ZSH_HIGHLIGHT_STYLES[path_approx]=fg=$ZSH_HIGHLIGHT_STYLES_PATH_APPROX

# PATH="$HOME/.perl5/bin${PATH+:}${PATH}"; export PATH;
# PERL5LIB="$HOME/.perl5/lib/perl5${PERL5LIB+:}${PERL5LIB}"; export PERL5LIB;
# PERL_LOCAL_LIB_ROOT="$HOME/.perl5${PERL_LOCAL_LIB_ROOT+:}${PERL_LOCAL_LIB_ROOT}"; export PERL_LOCAL_LIB_ROOT;
# PERL_MB_OPT="--install_base \"$HOME/.perl5\""; export PERL_MB_OPT;
# PERL_MM_OPT="INSTALL_BASE=$HOME/.perl5"; export PERL_MM_OPT;
