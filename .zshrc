# show git branch
function parse_git_branch() {
    git branch 2> /dev/null | sed -n -e 's/^\* \(.*\)/(\1)/p'
}

COLOR_DEF=$'%f'
COLOR_USR=$'%F{blue}'
COLOR_DIR=$'%F{yellow}'
COLOR_GIT=$'%F{red}'
setopt PROMPT_SUBST
export PROMPT='${COLOR_USR}%n ${COLOR_DIR}%~ ${COLOR_GIT}$(parse_git_branch)${COLOR_DEF} $ '

PROMPT=${PROMPT}'%(1j.[%j] .)'

source ~/.aliases

source ~/.fzf.zsh

eval "$(/opt/homebrew/bin/brew shellenv)"

source ~/.nvm/nvm.sh
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

# Add pyenv to PATH
export PATH="$HOME/.pyenv/bin:$PATH"

# Initialize pyenv in your shell
eval "$(pyenv init --path)"
eval "$(pyenv init -)"

HISTFILE=$HOME/.zsh_history
setopt HIST_IGNORE_SPACE
