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

# Aliases.
alias ,,,,='urxvtc && urxvtc && urxvtc && urxvtc'
alias ,,,='urxvtc && urxvtc && urxvtc'
alias ,,='urxvtc && urxvtc'
alias ,='urxvtc'
alias ab='acpi -b'
# Send 'urgent' bell to terminal. Useful to append at the end of commands that
# are expected to take a relatively significant amount of time.
alias bl='echo -e "\a"'
alias c='cat'
alias cb='cp'
alias cn='cat -n'
alias cp='cp -v'
alias cpr='cp -r'
alias cr='crystal'
# `nl` apparently more standard than `cat -n`.
alias ds='dirs -p | tail -n +2 | nl'
# TODO: Figure out a command that only deletes up to the *oldest* (?) current
# directory listing. Probably even make the 'k' aliases make use of it.
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
alias gcvu='git commit -m "chore(vendor): update"'
alias gd='git diff'
alias gds='git diff --staged'
# Edit 'dirty' files (in relation to their git status) with $EDITOR.
alias ge='git status --short | while read first rest; do if [[ $first == *"M"* ]] || [[ $first == *"A"* ]]; then echo $rest; fi; done | awk '"'"'{print $NF}'"'"' | while read gspath; do [[ ${gspath: -1} == "/" ]] || echo $gspath; done | while read filepath; do [[ -e $filepath ]] && echo $filepath; done | while read existingfile; do [[ "$(file -bL $existingfile)" == *"text"* ]] && echo $existingfile; done | xargs bash -c '"'"'</dev/tty $EDITOR "$@"'"'"' i'
alias gi='grep -i'
alias gir='grep -ir'
alias gl='git log'
# List authors/contributors and their email addresses for a git repository.
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
alias gshd='git show HEAD'
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
alias ln='ln -v'
alias ls='ls -p --color=auto'
alias ltr='ls -trp --color=auto'
alias md='mkdir -pv'
alias mi='mediainfo'
alias ml='mpv --loop=inf'
alias mn='mpv --no-video --loop=inf'
alias mv='mv -v'
alias ncmpcpp='ncmpcpp -c ~/.config/ncmpcpp/config'
alias npm='npm --no-color'
alias p8='ping 8.8.8.8'
alias pd='popd > /dev/null'
alias pdd='popd > /dev/null && popd > /dev/null'
alias pddd='popd > /dev/null && popd > /dev/null && popd > /dev/null'
alias pfd='popd > /dev/null && popd > /dev/null && popd > /dev/null && popd > /dev/null'
alias pffd='popd > /dev/null && popd > /dev/null && popd > /dev/null && popd > /dev/null && popd > /dev/null'
alias psd='popd > /dev/null && popd > /dev/null && popd > /dev/null && popd > /dev/null && popd > /dev/null && popd > /dev/null'
alias pn='ping -c 3 8.8.8.8'
# Useful in environments where internet connection is not very stable and drops
# randomly. Alerts when internet connection is back up.
alias pt='ping -c 3 8.8.8.8; while [[ $? -eq 1 ]]; do ping -c 3 8.8.8.8 && echo -e "\a"; done'
alias q='exit'
alias rm='rm -vI'
alias rmdir='rmdir -v'
alias rr='ranger'
alias s='sed'
# TODO: Figure out a more 'graceful' method of stopping mpd (it probably
# exists).
alias sd='mpc stop; mpd --kill; sudo shutdown now'
alias se='sudoedit'
alias sm='ls ~/music | grep -i'
alias t='tail'
# The single quotes in the search patterns ensure that the line itself is not
# listed in the search results.
alias todo='git grep -I -A 2 -e T''ODO: -e F''IXME:'
alias v='vim'
alias vc='vim ~/.vimrc'
alias vz='vim ~/.zshrc'
alias z='zathura --fork > /dev/null 2>&1'
alias zz='. ~/.zshrc'

function - {
  if [[ $# -eq 0 ]]; then
    cd "$OLDPWD"
  else
    builtin - "$@"
  fi
}

# 10ms delay (instead of default 400) for key sequences.
KEYTIMEOUT=1

HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000000

eval `dircolors ~/.dircolors`
source ~/.zcolors

# Helper for setting color including all kinds of terminals.
set_prompt_color () {
  if [[ $TERM = "linux" ]]; then
    # Do nothing.
    :
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
