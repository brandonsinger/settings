# /etc/skel/.bashrc
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !

alias e='zile'

function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
}
function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}
#export PS1='\[\033[01;32m\]\u@\h\[\033[00m\] \[\033[1;34m\]\w\[\033[0m\]$(parse_git_branch)$ '
export PS1="\[\e]2;\u@\h:\W\a\]\[\e[1;32m\]\u\[\e[0;32m\]@\h \[\e[0;36m\][\@] \[\e[1;34m\]\w\[\e[0m\]\\$ "

# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi


# Put your fun stuff here.


