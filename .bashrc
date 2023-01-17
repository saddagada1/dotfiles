#
# ~/.bashrc
#

export TERM="xterm-256color"
export HISTCONTROL=ignoredups:erasedups
export PATH="$HOME/.local/bin:$PATH"

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# replace ls with exa
alias ls='exa -al --color=always --group-directories-first' # my preferred listing
alias la='exa -a --color=always --group-directories-first'  # all files and dirs
alias ll='exa -l --color=always --group-directories-first'  # long format
alias lt='exa -aT --color=always --group-directories-first' # tree listing

# confirm before overwriting
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

# terminal rick roll
alias rr='curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'

# neofetch colorized
alias neofetch='neofetch | lolcat'

# uncomment if not using starship
#PS1='[\u@\h \W]\$ '

# random colorscript on startup & clear
# colorscript random
alias clear='clear; fortune | cowsay -f bong | lolcat'

# init starship
eval "$(starship init bash)"
