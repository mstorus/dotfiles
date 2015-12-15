export EDITOR="/usr/local/bin/emacs --no-init-file"

# show git branch
parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
export PS1="\[\033[00m\]\u@\h\[\033[01;33m\] \w \[\033[31m\]\$(parse_git_branch)\[\033[00m\]"

# show number of suspended jobs                                                                                                                      
export PS1=${PS1}'$([ \j -gt 0 ] && echo [\j])\$ '

source ~/.bash_aliases
