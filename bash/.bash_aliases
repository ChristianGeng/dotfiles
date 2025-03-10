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
