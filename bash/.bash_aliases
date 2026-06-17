# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# get fingerprint from file
alias fingerprint="ssh-keygen -l -E md5 -f"

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias o='less'
alias intellij="/home/christian/idea-IC-193.5662.53/bin/idea.sh"
alias java8-sdk="sdk use java 8.0.222.j9-adpt"

alias telegram=/opt/telegram/Telegram

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOMoh-my-zsh/custom/. folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias q="exit"
alias e="eject"
# alias octave_gui="flatpak run org.octave.Octave --gui"
# alias octave="flatpak run org.octave.Octave"
# das war alt. für dem Kernel brauche ich das das cli:
# export OCTAVE_EXECUTABLE=/home/christian/bin/octave

alias octave="/usr/bin/octave"
OCTAVE_EXECUTABLE=`which octave-cli`
export OCTAVE_EXECUTABLE
alias dolphin="XDG_CURRENT_DESKTOP=kde dolphin"

alias mem="egrep 'MemTotal|MemFree|MemAvailable' /proc/meminfo; vmstat; free"

alias clear_gradle_cache="rm -rf $HOME/.gradle/caches/"
alias pdfx="wine64 ~/bin/pdfx/PDFXCview.exe &>/dev/null"
alias zap="java -jar /home/audeering.local/cgeng/bin/ZAP_2.10.0/zap-2.10.0.jar"

# Emacs profile aliases
alias emacs-doom='emacs --with-profile doom'
alias emacs-default='emacs --with-profile default'
alias emacs-spacemacs='emacs --with-profile spacemacs'


# Claude code and vs studio
alias claude-work="CLAUDE_CONFIG_DIR=~/.claude-work/ claude"
alias claude-personal="CLAUDE_CONFIG_DIR=~/.claude-personal/ claude"


# --- Host-based kitty background tint --------------------------------------
# When you run ssh from a LOCAL kitty window, tint the background based on the
# host (mapping in ~/.config/kitty/ssh-themes), and reset on disconnect.
# No-op outside kitty, or when remote control / the kitten binary is absent --
# so it's safe even though this file is sourced on remote servers too.
_kitty_theme_for() {
  # Echo the theme file for the first pattern matching the resolved HostName
  # (robust to ssh options) or the typed target. Returns non-zero if no match.
  local resolved="$1" typed="$2" pat file
  [ -f ~/.config/kitty/ssh-themes ] || return 1
  while read -r pat file; do
    case "$pat" in ''|\#*) continue ;; esac
    case "$resolved" in $pat) echo "$HOME/.config/kitty/themes/$file"; return 0 ;; esac
    case "$typed"    in $pat) echo "$HOME/.config/kitty/themes/$file"; return 0 ;; esac
  done < ~/.config/kitty/ssh-themes
  return 1
}

ssh() {
  # Remote-control entry point: `kitten @` (newer kitty) or `kitty @` (universal,
  # works on older kitty where `@` isn't a standalone kitten).
  local kc=""
  if command -v kitten >/dev/null 2>&1; then kc="kitten @"
  elif command -v kitty >/dev/null 2>&1; then kc="kitty @"; fi
  if [ -z "$KITTY_WINDOW_ID" ] || [ -z "$kc" ]; then
    command ssh "$@"; return
  fi
  # Resolved hostname (parses all ssh options correctly) + best-effort typed target.
  local resolved typed="" a
  resolved="$(command ssh -G "$@" 2>/dev/null | awk '/^hostname /{print $2; exit}')"
  for a in "$@"; do case "$a" in -*) ;; *) typed="${a#*@}"; break ;; esac; done
  local theme; theme="$(_kitty_theme_for "$resolved" "$typed")"
  # No --all: color only THIS window/tab (kitty's default scope), so each tab
  # keeps the color of the host it connected to.
  [ -n "$theme" ] && [ -f "$theme" ] && $kc set-colors "$theme" 2>/dev/null
  command ssh "$@"
  [ -n "$theme" ] && $kc set-colors --reset 2>/dev/null
}
