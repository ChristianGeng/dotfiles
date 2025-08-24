# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi

unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
    xterm*|rxvt*)
        PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
        ;;
    *)
        ;;
esac

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if [ -f ~/.bash_path ]; then
    echo "sourcing existing bash_path in home directory"
    . ~/.bash_path
fi

if [ -f ~/.bash_variables ]; then
    . ~/.bash_variables
fi

if [ -f ~/.shell_scripts ]; then
    . ~/.shell_scripts
fi

if [ -f ~/.docker_scripts ]; then
    . ~/.docker_scripts
fi

# audeering config files maintaned in specific repo
if [ -f ~/.audeering_configs ]; then
    . ~/.audeering_configs
fi

# local config files not maintaned here
if [ -f ~/.local_configs ]; then
    . ~/.local_configs
fi

# .pyenv, see https://github.com/pyenv
if [ -f ~/.pyenv ]; then
    . ~/.pyenv
fi

# my private gpg encrypted secrets also tangled
# if [ -f ~/.bash_secrets ]; then
#     . ~/.bash_secrets
# fi

if [ -f ~/.ansible_hooks ]; then
    . ~/.ansible_hooks
fi

# echo "Reading secrets from pass store"
# if [ -f ~/.bash_secrets ]; then
#     . ~/.bash_secrets
# fi


# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi


# function workon(){
#     cmd="/home/christian/.venvs/"${1}"/bin/activate"
#     echo cmd
# }

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/christian/.sdkman"
[[ -s "/home/christian/.sdkman/bin/sdkman-init.sh" ]] && source "/home/christian/.sdkman/bin/sdkman-init.sh"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


export MKL_THREADING_LAYER=GNU
export "THEANO_FLAGS='gcc.cxxflags=-Wno-c++11-narrowing'"
export MKL_THREADING_LAYER=GNU
export "THEANO_FLAGS='gcc.cxxflags=-Wno-c++11-narrowing'"
export MKL_THREADING_LAYER=GNU
export "THEANO_FLAGS='gcc.cxxflags=-Wno-c++11-narrowing'"
export MKL_THREADING_LAYER=GNU
export "THEANO_FLAGS='gcc.cxxflags=-Wno-c++11-narrowing'"

export EDITOR=emacsclient

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/opt/conda/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "/opt/conda/etc/profile.d/conda.sh" ]; then
#         . "/opt/conda/etc/profile.d/conda.sh"
#     else
#         export PATH="/opt/conda/bin:$PATH"
#     fi
# fi
# unset __conda_setup
# <<< conda initialize <<<

# ~/.pyenvrc
# eval "$(pyenv virtualenv-init -)"
#
#
if [ -f "$HOME/.cargo/env" ]; then
   . "$HOME/.cargo/env"
fi
