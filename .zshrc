ZSH=$HOME/.oh-my-zsh
ZSH_THEME="lambda"
plugins=(battery git git-flow github heroku lein lol yum zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

zstyle :omz:plugins:ssh-agent agent-forwarding on
zstyle :omz:plugins:ssh-agent identities id_rsa

export PROMPT='λ %~/ %n@%M $(git_prompt_info)%{$reset_color%}'

if [[ "$TERM" == "dumb" ]]
then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  # unfunction precmd
  # unfunction preexec
  PS1='$ '
fi

# Customize to your needs...
# Thanks, Marshall: <http://yountlabs.com/automation/disable-autocorrect-in-zsh/>.
unsetopt correct_all

# Pathdirs: <http://stackoverflow.com/a/9352979>; and another:
# <http://unix.stackexchange.com/a/31235>.
path=
path+=$HOME/bin
path+=/usr/games
path+=/usr/lib64/qt4/bin/
path+=/usr/local/apache2/bin
path+=/usr/local/apr/bin
path+=/usr/local/cling/bin
path+=/usr/local/ec2/bin
path+=/usr/local/hadoop/bin
path+=/usr/local/hadoop/sbin
path+=/usr/local/java/bin
path+=/usr/local/lib/surfraw
path+=/usr/local/maven/bin
path+=/usr/local/mongo/bin
path+=/usr/local/mysql/bin
path+=/usr/local/neo4j/bin
path+=/usr/local/pgsql/bin
path+=/usr/local/ssl/bin
path+=/usr/local/xulrunner
path+=$HOME/src/stumpwm/contrib
path+=$HOME/lib/go/bin
path+=$HOME/google-cloud-sdk/bin
path+=/usr/local/sbin
path+=/usr/sbin
path+=/sbin
path+=/usr/local/bin
path+=/usr/bin
path+=/bin
path+=$HOME/.config/surfraw/elvi

export ACLOCAL="aclocal -I /usr/local/share/aclocal"
# We're doing it wrong.
export ANT_ARGS="$ANT_ARGS -emacs"
export ANT_HOME=/usr/local/ant
export ANT_OPTS="$ANT_OPTS -Dbuild.compiler.emacs=true"
# For Java apps under Ratpoison
# export AWT_TOOLKIT=MToolkit
export CATALINA_HOME=/usr/local/tomcat
export CLASSPATH="${HOME}/lib/java/*"
export CLOJURE_HOME=/usr/local/clojure
export CVS_RSH=ssh
export EC2_HOME=/usr/local/ec2
# export EDITOR='emacs-no-wait'
export EDITOR='emacs-wait'
# Use the hard-coded framebuffer font.
export FBFONT=fim://
export GOPATH=~/prg/go
export GOROOT=~/lib/go
export GOOS=linux
export GOARCH=amd64
export GOBIN=~/bin
export GROFF_NO_SGR=1
export HADOOP_PREFIX=/usr/local/hadoop
export HADOOP_COMMON_HOME=$HADOOP_PREFIX
export HADOOP_HDFS_HOME=$HADOOP_PREFIX
export HADOOP_MAPRED_HOME=$HADOOP_PREFIX
export HADOOP_YARN_HOME=$HADOOP_PREFIX
export HADOOP_CONF_DIR=$HADOOP_PREFIX/etc/hadoop
export YARN_CONF_DIR=$HADOOP_PREFIX/etc/hadoop
# export JAVA_HOME=/usr/local/java
# For compatibility with IPv4.
export JAVA_OPTS="-Djava.net.preferIPv4Stack=true"
# export HADOOP_HOME=/usr/local/hadoop
export LANG=en_US.UTF-8
export LESS='-i'
export M4PATH="/usr/local/share/aclocal"
export MBOX=~/mail/received
export PGDATA=~/var/pgsql
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig:/usr/lib/pkgconfig:/usr/share/pkgconfig:/usr/local/ssl/lib/pkgconfig:$PKG_CONFIG_PATH
# export R_HOME=/usr/local/lib64/R
export R_LIBS=~/lib/R
export RUNE_DATA_PATH=/usr/local/games/rune/System
export SGML_CATALOG_FILES=/etc/sgml/sgml-docbook.cat
export SISC_HOME=/usr/local/sisc
# export TERM=xterm-256color
export TMPDIR=/tmp
export XCOMPOSEFILE=~/.Xcompose

alias ....='cd ../../..'    # And for good measure
alias ...='cd ../..'        # Go up two directories
alias ..='cd ..'            # Go up one directory
alias a=ack-grep
alias b=blaze
alias c='cd'
alias ca=cat
alias cp='cp -v'
alias cs='chicken-install -s'
alias dc='dc -e "10 k" -'
alias dh='dirs -v'
alias e='emacs-no-wait'
alias f='find'
# alias fetchmail='fetchmail --nodetach'
alias ftp='ftp -iv'
# `g&g' (as suggested by huffshell) doesn't work as an alias.
# alias 'g&g'='gcom && gl'
alias g5=git5
alias gcam='git ci -a -m'
alias gcom='gco master'
alias gg='gcom && gl'
# alias google-chrome-proxy='google-chrome --proxy-server=localhost:8118'
# alias google-chrome='google-chrome --enable-plugins'
alias grep='grep --color=always'
alias h='history'
alias i='elinks'
alias k='klutometis'
alias l.='l -d .*'
# alias l='ls -lah'           # Long view, show hidden
alias l='ls -hF --color=auto'
# alias la='ls -AF'           # Compact view, show hidden
alias la='ll -Atrc'
alias le='less'
alias less='less -i -R'
# alias ll='ls -lFh'          # Long view, no hidden
alias ll='l -l'
alias ln='ln -v'
alias m='mutt'
alias mkdir='mkdir -v'
alias mm='/usr/bin/mutt'
alias mv='mv -v'
alias n='newsbeuter'
alias p='prodaccess'
alias pwgen='pwgen -s -y 14'
alias pwgen='pwgen -s -y 14'
alias rm='rm -v'
# From <http://askubuntu.com/a/22043>: allows aliases to be used with
# sudo.
alias s='sudo'
alias sac0='sudo alsamixer -c 0'
alias sai='sudo apt-get install'
alias sas='sudo apt-cache search'
alias sls='screen -ls'
alias sncs='sudo nmcli con status'
alias sncui='sudo nmcli con up id'
alias srd='screen -RD'
alias sudo='sudo '
alias sy='sudo yum'
alias syi='sudo yum install'
alias sys='sudo yum search'
alias t.='tree -a'
alias t='tar'
# alias t='tree'
alias td='tree -d'
alias tf='tree -f'
alias tree='tree -CF'
alias tt='tree -t'
alias wh=which

unsetopt CASE_GLOB
setopt NO_NOMATCH

if [ -f ~/.$(hostname -s).sh ]; then
        source ~/.$(hostname -s).sh
fi
