alias gb='git rev-parse --abbrev-ref HEAD'
alias gfr='git fetch origin && git reset --hard origin/master'
alias git-branch-recent='for k in `git branch | sed s/^..//`; do echo -e `git log -1 --pretty=format:"%ci %cr" $k --`\\t"$k";done | sort'
alias emacs='emacs -nw'
alias e='emacsclient -a "" -t'
alias kill-emacs='emacsclient -e "(kill-emacs)"'
alias ag="ag $* --pager 'less -R'"
