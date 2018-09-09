# Copyright (C) 2018  Zhijin Li

# All rights reserved.

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

#     * Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above
# copyright notice, this list of conditions and the following disclaimer
# in the documentation and/or other materials provided with the
# distribution.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

## ---------------------------------------------------------------------------
##
## File: .bashrc for config-files
##
## Created by Zhijin Li
## E-mail:   <jonathan.zj.lee@gmail.com>
##
## Started on  Sun Sep  9 21:24:31 2018 Zhijin Li
## Last update Sun Sep  9 21:35:45 2018 Zhijin Li
## ---------------------------------------------------------------------------
# .bashrc


# Colors
RED='\[\e[1;31m\]'
GREEN='\[\e[1;32m\]'
PURPLE='\[\e[0;35m\]'
RED='\[\e[1;31m\]'
blue='\[\e[0;34m\]'
BLUE='\[\e[1;34m\]'
cyan='\[\e[0;36m\]'
CYAN='\[\e[1;36m\]'
YELLOW='\[\e[0;33m\]'
WHITE='\[\e[0m\]'
END='\[\e[m\]'


# Aliases
export TERM=xterm-256color
export VISUAL="emacs"
export EDITOR="$VISUAL"
export IDE="emacs"
export BROWSER="firefox"

alias e=$IDE
alias ij="imagej -c -x 4000"

alias ec="emacsclient -c"
alias ed="emacs --daemon"

LS_COLORS=$LS_COLORS:'ow=1;4;34;100'
export LS_COLORS

alias ls='ls --color=auto -Fh'
alias ll="ls -al"
alias ..='cd ..'
alias hist="grep '$1' ~/.bash_history"
alias mem="free -m"
alias sshfs='sshfs -C -o reconnect -o workaround=all'

alias ip3="python3 $(which ipython3)"


# Network Proxy
export http_proxy=
export https_proxy=$http_proxy
export ftp_proxy=$http_proxy
export rsync_proxy=$http_proxy
export no_proxy="localhost,127.0.0.1,localaddress,.localdomain.com"


# Determine active Python virtualenv details.
function set_virtualenv()
{
    if test -z "$VIRTUAL_ENV" ; then
        PYTHON_VIRTUALENV=""
    else
        PYTHON_VIRTUALENV="${BLUE}[`basename \"$VIRTUAL_ENV\"`]${END} "
    fi
}

# PROMPT
function parse_git_branch()
{
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

function prompt_cmd
{
    set_virtualenv
    UNIFO="$RED[$END$BLUE\u$END$RED@$END$GREEN\h$END$RED]$END$RED:$END $PURPLE\w$END"
    PS1=${PYTHON_VIRTUALENV}"$YELLOW[\t]$END$UNIFO$YELLOW$(parse_git_branch)$END$WHITE\$ $END"
}

PROMPT_COMMAND=prompt_cmd


# Kill emacs daemon server
function kill_emacs()
{
    emacsclient -e '(kill-emacs)'
}


# For git cmd auto-completion.
if [ -f "${HOME}/.git-completion.bash" ]; then
    source ${HOME}/.git-completion.bash
fi


# Autocomplete behavior.
set menu-complete-display-prefix on
set colored-complete-prefix on
set skip-completed-text on

bind 'TAB:complete'
bind '\C-f:menu-complete'
bind '\C-b:menu-complete-backward'


# Environment variables
export LOCAL_DIR=${HOME}/local

export PATH=/usr/sbin:/usr/bin:/sbin:/bin
export PATH=/usr/local/sbin:/usr/local/bin:$PATH
export PATH=${LOCAL_DIR}/bin:$PATH
export PATH=${HOME}/.local/bin:$PATH

export LD_LIBRARY_PATH=/usr/local/lib:/usr/lib:/lib
export LD_LIBRARY_PATH=${LOCAL_DIR}/lib:${LD_LIBRARY_PATH}
export LD_LIBRARY_PATH=${HOME}/.local/lib:${LD_LIBRARY_PATH}
