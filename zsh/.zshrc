# STTY
# ====
# Disable start/stop signals (therefore allows to use ^s in keybindings).
stty -ixon

# Loads
# =====
autoload -U colors && colors
autoload -U deer
autoload edit-command-line
autoload -Uz vcs_info

# Options
# =======
# Use more interactive 'menu' style tab completion. :D
zstyle ':completion:*' menu select
# Used in $RPROMPT for showing current git branch, see prompt section.
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git*' formats "%b"

cdpath=(~)
setopt autocd extendedglob no_beep prompt_subst
# Disables annoying GTK authentication popup for example when doing `git push'.
unset SSH_ASKPASS
# Escape key delay to go into normal mode on the cli is 0.1s instead of default
# 0.4s.
KEYTIMEOUT=1

# History
# -------
# Lines of history to keep in memory.
HISTSIZE=2000
# Should come out to approximately 1GB max file size.. I think.
SAVEHIST=20000000
# Immediately append to $HISTFILE, instead of when the shell exits.
setopt inc_append_history
# If a new command line being added to the history list duplicates an older
# one, remove the older one from the history list, even if it is not the
# previous event.
setopt hist_ignore_all_dups
# Do not add command lines that begin with at least one space to the history
# list.
setopt hist_ignore_space
# Do not save duplicate entries into $HISTFILE.
setopt hist_save_no_dups

# Functions
# =========
# Send 'urgent' bell to terminal. Useful to append at the end of commands that
# are expected to take a long time to finish so that you can be notified when
# they're done.
bell () {
  echo -e "\a"
}

ping-google () {
  local dest='8.8.8.8'
  [[ -z "$1" ]] && ping "$dest" || ping -c "$1" "$dest"
}

ping-test () {
  ping-google 3
  while [[ $? -eq 1 ]]; do ping-google 3 && bell; done
}

# Enter directory.
ed () {
  mkdir "$1"
  cd ./"$1"
}

# Exit directory.
ex () {
  cd ..
  rmdir "$OLDPWD" >/dev/null
}

tw () {
  { fd -e tex -d 1; fd -e png; fd -e jpg; } \
    | entr -s 'latexmk -lualatex \
      && (mv -u .aux/*.pdf . >/dev/null 2>&1 || true) \
      && (ln -s $(readlink -f *.pdf) .aux/. >/dev/null 2>&1 || true)'
}

# Zle
# ---
# Helper function for setting cursor shape.
set-cursor-shape () {
  if [[ $TERM != "linux" ]]; then
    case "$1" in
      blinking_block)
        echo -e -n "\x1b[\x30 q"
        ;;
      steady_block)
        echo -e -n "\x1b[\x32 q"
        ;;
      blinking_underline)
        echo -e -n "\x1b[\x33 q"
        ;;
      steady_underline)
        echo -e -n "\x1b[\x34 q"
        ;;
      blinking_bar)
        echo -e -n "\x1b[\x35 q"
        ;;
      steady_bar)
        echo -e -n "\x1b[\x36 q"
        ;;
    esac
  fi
}

zle-keymap-select () {
  if [[ $KEYMAP == 'vicmd' ]]; then
    set-cursor-shape steady_block
  else
    set-cursor-shape steady_bar
  fi
}

zle-line-init () {
  zle -K viins
  set-cursor-shape steady_bar
}

# This helper function is used simply to reset the command line, so that any
# current directory prompt indicators can be updated after each cd command.
zle-cd () {
  cd "$1"
  zle kill-whole-line
  zle accept-line
}

go-home () {
  zle-cd ~
}

go-back () {
  zle-cd "$OLDPWD"
}

go-up () {
  zle-cd ..
}

# For opening a new terminal emulator in the current directory.
run-term-em () {
  # `&!' is a convenient shortcut for `& disown'.
  [[ $TERM != "linux" ]] && ${TERM%%-*} >/dev/null 2>&1 &!
}

zle -N edit-command-line
zle -N deer
zle -N zle-keymap-select
zle -N zle-line-init
zle -N go-home
zle -N go-back
zle -N go-up
zle -N run-term-em

# Key Bindings
# ============
bindkey -M vicmd '^g' deer
bindkey -M viins '^g' deer
bindkey -M vicmd 'v'  edit-command-line
bindkey -M vicmd '^j' go-back
bindkey -M viins '^j' go-back
bindkey -M vicmd '^h' go-home
bindkey -M viins '^h' go-home
bindkey -M vicmd '^k' go-up
bindkey -M viins '^k' go-up
bindkey -M vicmd '^s' history-incremental-search-backward
bindkey -M viins '^s' history-incremental-search-backward
bindkey -M vicmd '/'  history-incremental-search-backward
bindkey -M vicmd '^f' history-incremental-search-forward
bindkey -M viins '^f' history-incremental-search-forward
bindkey -M vicmd '?'  history-incremental-search-forward
bindkey -M viins '^u' reverse-menu-complete
bindkey -M vicmd '^o' run-term-em
bindkey -M viins '^o' run-term-em
bindkey -M vicmd 'H'  vi-beginning-of-line
bindkey -M vicmd 'L'  vi-end-of-line

# Aliases
# =======
alias ab='acpi -b'
alias am='alsamixer'
alias bl='bell'
alias c='cat'
alias cp='cp -v'
alias cpr='cp -r'
alias f='file'
alias g='git'
alias ga='git add'
alias gall='git add -A'
alias gap='git add -p'
alias gch='git checkout'
alias gcm='git commit'
alias gcmm='git commit --edit -m \
  "$(git log --format=%B --reverse HEAD..HEAD@{1})"'
alias gd='git diff'
alias gdc='git diff --check'
alias gds='git diff --staged'
alias gdsc='git diff --staged --check'
alias gg='git grep'
alias ggi='git grep -i'
alias gl='git log'
alias gld='git log -p'
alias gp='grep'
alias gpi='grep -i'
alias gpsh='git push'
alias grs='git reset'
alias gs='git status'
alias gsh='git show'
alias gshd='git show HEAD'
alias info='info --vi-keys'
alias l='less -R'
alias ls='ls -p --color'
alias l1='ls -1'
alias la='ls -A'
alias la1='ls -A1'
alias lah='ls -lAh'
alias lh='ls -lh'
alias m='mpv --loop=inf'
alias ma='mpv --loop=inf --no-video'
alias md='mkdir -pv'
alias mi='mediainfo'
alias mv='mv -v'
alias nixpaste="curl -F 'text=<-' http://nixpaste.lbr.uno"
alias pn='ping-google 3'
alias rm='rm -vI'
alias rmdir='rmdir -v'
alias sd='sudo shutdown now'
alias se='sudoedit'
# I basically only use stow to manage dotfiles.
alias stow='stow -t ~ --no-folding'
alias tar-extract='tar -xzvf'
# The single quotes in the search patterns ensure that the line itself is not
# listed in the search results.
alias todo='git grep -I -A 2 -e T''ODO: -e F''IXME:'
alias v='vim'
alias vc='vim ~/.vimrc'
alias vn='vim ~/dots/nixos/config.nix'
alias vs='vim ~/dots/sxhkd/.config/sxhkd/sxhkdrc'
alias vz='vim ~/.zshrc'
alias z='zathura --fork'
alias zz='. ~/.zshrc'

# Prompt
# ======
precmd () {
  vcs_info
}

local rsc="%{$reset_color%}"
PROMPT="$rsc %B%(?.%{$fg[green]%}.%{$fg[red]%})━━━$rsc%b "
RPROMPT='$rsc${BGJOBS}%(1j.%j.) $rsc%{$fg[magenta]%}${vcs_info_msg_0_} \
$rsc%B%{$fg[blue]%}%~%b$rsc'

# Theming
# =======
# Disable underlines.
ZSH_HIGHLIGHT_STYLES[precommand]='fg=green'
ZSH_HIGHLIGHT_STYLES[path]=none
ZSH_HIGHLIGHT_STYLES[path_prefix]=none
ZSH_HIGHLIGHT_STYLES[path_approx]='fg=yellow'
