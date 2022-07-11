#git
alias gs='git status'
alias gc='git commit -am $1'
alias gadd='git add --all :/'
alias gp='git pull --rebase $1'
#emacs
alias estart='emacs --daemon $@'
alias estop="emacsclient --eval '(kill-emacs)'"
alias ec="emacsclient -nw"
#docker
alias docker-kill-all='docker kill $(docker ps -q)'
alias docker-remove-all='docker rm $(docker ps -a -q) && docker rmi $(docker images -q)'
# icat
alias icat="kitty +kitten icat"
