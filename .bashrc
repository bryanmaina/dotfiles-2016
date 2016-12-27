#
# ~/.bashrc
#

# https://s3.amazonaws.com/udacity-hosted-downloads/lesson1.zip

# If not running interactively, don't do anything
[[ $- != *i* ]] && return


export JAVA_HOME="/usr/lib/jvm/java-8-openjdk/"
export lVISUAL="nvim"
export EDITOR="nvim"
export BROWSER="/usr/bin/chromium"
export AWT_TOOLKIT=XToolkit
export NOTES_DIR="/home/dumy/Documents/Cours_UCL/"
export XDG_CONFIG_HOME="/home/dumy/.config/"
export ANDROID_HOME="/opt/android-sdk"
#export PATH="$PATH:$HOME/.node/bin"
export PATH="$HOME/.node_modules/bin:$PATH"
export npm_config_prefix=~/.node_modules

# setting fzf finder
export FZF_DEFAULT_COMMAND='ag -g ""'

if which ruby >/dev/null && which gem >/dev/null; then
    PATH="$(ruby -rubygems -e 'puts Gem.user_dir')/bin:$PATH"
fi

# Git bash prompt
source /usr/share/git/completion/git-prompt.sh
GIT_PS1_SHOWSTASHSTATE=true
GIT_PS1_SHOWDIRTYSTATE=true


alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias grep='grep --color=tty -d skip'
alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias np='nano PKGBUILD'
alias fixit='sudo rm -f /var/lib/pacman/db.lck && sudo pacman-mirrors -g && sudo pacman -Syyuu  &&
sudo pacman -Suu'
alias wine32='env WINEARCH=win32 WINEPREFIX="/home/dumy/.wine32" wine'
alias League_of_Legends='pushd /home/dumy/.local/share/leagueoflegends/LOL/RADS/system/ && wine32 rads_user_kernel.exe run lol_launcher $(ls ../projects/lol_launcher/releases/) LoLLauncher.exe && popd'

alias ls='ls --color=auto'

alias v='nvim'
alias emacs='emacs -nw'

# Getting the weather of any city
wego ()
{
  curl http://wttr.in/"$1"
}

# ex - archive extractor
# usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

#convert audios
webmTOmp4 () {
  ffmpeg -i "$1".webm -qscale 0 "$1".mp4
}
mp4TOmp3 () {
  ffmpeg -i "$1".mp4 "$1".mp3
}


#return value visualisation
 PS1="\$(if [[ \$? == 0 ]]; then echo \"\[\033[34m\]\"; else echo \"\[\033[31m\]\"; fi)\342\226\210\342\226\210 [ \W ] \[\033[0;31m\] [ \t ]\$(__git_ps1 ' (%s)')\n\[\033[m\]\342\226\210\342\226\210 "

#PS1="$(if [[ ${EUID} == 0 ]]; then echo '\[\033[01;31m\]\u@\h'; else echo '\[\033[01;32m\]\u@\h'; fi)\[\033[01;34m\] \w \[\033[01;33m\]\n \$(__git_ps1 ' (%s)') \[\033[01;34m\]\$([[ \$? != 0 ]] && echo \"\[\033[01;31m\]:(\[\033[01;34m\] \")\\$\[\033[00m\] "

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
###-begin-ng-completion###
#
# ng command completion script
#
# Installation: ng completion >> ~/.bashrc (or ~/.zshrc)
#

ng_opts='b build completion doc e2e g generate get github-pages:deploy gh-pages:deploy h help i init install lint make-this-awesome new s serve server set t test v version -h --help'

build_opts='--aot --base-href --environment --output-path --suppress-sizes --target --watch --watcher -bh -dev -e -o -prod -t -w'
generate_opts='class component directive enum module pipe route service c cl d e m p r s --help'
github_pages_deploy_opts='--base-href --environment --gh-token --gh-username --message --skip-build --target --user-page -bh -e -t'
help_opts='--json --verbose -v'
init_opts='--dry-run inline-style inline-template --link-cli --mobile --name --prefix --routing --skip-bower --skip-npm --source-dir --style --verbose -d -is -it -lc -n -p -sb -sd -sn -v'
new_opts='--directory --dry-run inline-style inline-template --link-cli --mobile --prefix --routing --skip-bower --skip-git --skip-npm --source-dir --style --verbose -d -dir -is -it -lc -p -sb -sd -sg -sn -v'
serve_opts='--aot --environment --host --live-reload --live-reload-base-url --live-reload-host --live-reload-live-css --live-reload-port --port --proxy-config --ssl --ssl-cert --ssl-key --target --watcher -H -e -lr -lrbu -lrh -lrp -p -pc -t -w'
set_opts='--global -g'
test_opts='--browsers --build --colors --log-level --port --reporters --watch -w'
version_opts='--verbose'

if type complete &>/dev/null; then
  _ng_completion() {
    local cword pword opts

    COMPREPLY=()
    cword=${COMP_WORDS[COMP_CWORD]}
    pword=${COMP_WORDS[COMP_CWORD - 1]}

    case ${pword} in
      ng) opts=$ng_opts ;;
      b|build) opts=$build_opts ;;
      g|generate) opts=$generate_opts ;;
      gh-pages:deploy|github-pages:deploy) opts=$github_pages_deploy_opts;;
      h|help|-h|--help) opts=$help_opts ;;
      init) opts=$init_opts ;;
      new) opts=$new_opts ;;
      s|serve|server) opts=$serve_opts ;;
      set) opts=$set_opts ;;
      t|test) opts=$test_opts ;;
      v|version) opts=$version_opts ;;
      *) opts='' ;;
    esac

    COMPREPLY=( $(compgen -W '${opts}' -- $cword) )

    return 0
  }

  complete -o default -F _ng_completion ng
elif type compctl &>/dev/null; then
  _ng_completion () {
    local words cword opts
    read -Ac words
    read -cn cword
    let cword-=1

    case $words[cword] in
      ng) opts=$ng_opts ;;
      b|build) opts=$build_opts ;;
      g|generate) opts=$generate_opts ;;
      gh-pages:deploy|github-pages:deploy) opts=$github_pages_deploy_opts ;;
      h|help|-h|--help) opts=$help_opts ;;
      init) opts=$init_opts ;;
      new) opts=$new_opts ;;
      s|serve|server) opts=$serve_opts ;;
      set) opts=$set_opts ;;
      t|test) opts=$test_opts ;;
      v|version) opts=$version_opts ;;
      *) opts='' ;;
    esac

    setopt shwordsplit
    reply=($opts)
    unset shwordsplit
  }

  compctl -K _ng_completion ng
fi

###-end-ng-completion###

