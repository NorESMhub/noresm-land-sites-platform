# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
alias view='vim -R'
alias du='du -h'
alias df='df -h'
alias grep='egrep --color=always -n --exclude-dir=.git'
alias tom='top -o %MEM'
alias ncdump='ncdump -h'
alias jlremote='jupyter lab --no-browser --port=8899'
