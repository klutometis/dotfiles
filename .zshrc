# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="linuxonly"           # Doesn't seem to work.
ZSH_THEME="terminalparty"
ZSH_THEME="simple"
ZSH_THEME="lambda"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(battery git git-flow github heroku lein lol ssh-agent yum zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

export PROMPT='λ %~/ %n@%m $(git_prompt_info)%{$reset_color%}'

if [[ "$TERM" == "dumb" ]]
then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  unfunction precmd
  unfunction preexec
  PS1='$ '
fi

# Customize to your needs...
# Thanks, Marshall: <http://yountlabs.com/automation/disable-autocorrect-in-zsh/>.
unsetopt correct_all

export PATH=/home/peter/bin:/usr/local/ec2/bin:/usr/local/apr/bin:/usr/local/neo4j/bin:/usr/local/ssl/bin:/usr/local/java/bin:/usr/local/maven/bin:/usr/local/apache2/bin:/usr/local/mysql/bin:/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin

export RUNE_DATA_PATH=/usr/local/games/rune/System
export SISC_HOME=/usr/local/sisc
export CATALINA_HOME=/usr/local/tomcat
export JAVA_HOME=/usr/local/java
export ANT_HOME=/usr/local/ant
# For compatibility with IPv4.
export JAVA_OPTS="-Djava.net.preferIPv4Stack=true"
export GROFF_NO_SGR=1
export CVS_RSH=ssh
export SGML_CATALOG_FILES=/etc/sgml/sgml-docbook.cat
export EDITOR='emacs-no-wait'
# For Java apps under Ratpoison
# export AWT_TOOLKIT=MToolkit
export LANG=en_US.UTF-8
export LESS='-i'
export MBOX=~/mail/received
export ANT_ARGS="$ANT_ARGS -emacs"
export ANT_OPTS="$ANT_OPTS -Dbuild.compiler.emacs=true"
export ACLOCAL="aclocal -I /usr/local/share/aclocal"
export M4PATH="/usr/local/share/aclocal"
export GOROOT=~/lib/go
export GOOS=linux
export GOARCH=amd64
export GOBIN=~/bin
export R_HOME=/usr/local/lib64/R
export R_LIBS=~/lib/R
export EC2_HOME=/usr/local/ec2
export AWS_ACCESS_KEY=AKIAJ6GTV2NEDUI3MY6Q
export AWS_SECRET_KEY=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
export CLOJURE_HOME=/usr/local/clojure
export CLASSPATH="/home/pcdanenb/lib/java/*"
# export HADOOP_HOME=/usr/local/hadoop
# export GOOGLE_KEY=AIzaSyDXfOh6HUK1ImU1oVsM0bFsKOpbcEjlW7g
# export TERM=xterm-256color
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig:/usr/lib/pkgconfig:/usr/share/pkgconfig:$PKG_CONFIG_PATH
export TMPDIR=/tmp
export XCOMPOSEFILE=~/.Xcompose

alias fetchmail='fetchmail --nodetach'
alias pwgen='pwgen -s -y 14'
alias ..='cd ..'            # Go up one directory
alias ...='cd ../..'        # Go up two directories
alias ....='cd ../../..'    # And for good measure
alias l='ls -lah'           # Long view, show hidden
alias la='ls -AF'           # Compact view, show hidden
alias ll='ls -lFh'          # Long view, no hidden

alias dh='dirs -v'

unsetopt CASE_GLOB
setopt NO_NOMATCH

# User specific aliases and functions
alias less='less -i -R'
#alias mutt='ssh -t wikitex.org ". .bashrc; mutt"'
alias ftp='ftp -iv'
# alias google-chrome='google-chrome --enable-plugins'
# alias google-chrome-proxy='google-chrome --proxy-server=localhost:8118'
alias dc='dc -e "10 k" -'

alias ls='ls -hF --color=auto'
alias ll='ls -lFh  --color=auto'
alias la='ls -AF --color=auto'
alias l='ls -CF --color=auto'
alias l.='ls -d .* --color=auto'
alias pwgen='pwgen -s -y 14'
alias cp='cp -v'
alias mv='mv -v'
alias ln='ln -v'
alias rm='rm -v'
alias grep='grep --color=always'

alias tree='tree -CF'
alias t='tree'
alias tt='t -t'
alias td='t -d'
alias t.='t -a'
alias tf='t -f'

# Some suggestions from huffshell:
alias s='sudo'
alias sy='sudo yum'
alias syi='sudo yum install'
alias m='mutt'
alias c='cd'
alias k='klutometis'
alias e='emacs-no-wait'
alias gcam='git ci -a -m'
alias gcom='gco master'
alias cs='chicken-install -s'
alias le='less'

# Copy/kill region to X. Thanks, Gilles! See
# <http://unix.stackexchange.com/a/18704>.
if [[ -n $DISPLAY ]]; then
  x-copy-region-as-kill () {
    zle copy-region-as-kill
    print -rn -- "$CUTBUFFER" | xsel -ib
  }
  x-kill-region () {
    zle kill-region
    print -rn -- "$CUTBUFFER" | xsel -ib
  }
  zle -N x-copy-region-as-kill
  zle -N x-kill-region
  bindkey '\C-w' x-kill-region
  bindkey '\ew' x-copy-region-as-kill
fi
